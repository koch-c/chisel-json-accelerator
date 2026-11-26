import chisel3._
import chisel3.util._

/** Addresses and lengths are measured in bytes. Quotes, if present, are data.
  */
class AttributeDescriptor extends Bundle {
  val attributeId = UInt(8.W)
  val startAddress = UInt(12.W)
  val length = UInt(13.W) // Can represent the entire 4096-byte buffer.
}

class AttributeByte extends Bundle {
  val data = UInt(8.W)
  val attributeId = UInt(8.W)
  val last = Bool()
}

// Attribute word is whole word alongside some metadata.
//     - data:        the whole word, lane 0 in bits 7:0
//     - attributeId: same meaning as in AttributeByte
//     - firstByte:   lane of the first byte that belongs to the attribute
//     - lastByte:    lane of the last byte that belongs to the attribute
//     - last:        this is the final word of the attribute

class AttributeWord extends Bundle {
  val data = UInt(64.W)
  val attributeId = UInt(8.W)
  val firstByte = UInt(3.W)
  val lastByte = UInt(3.W)
  val last = Bool()
}

/** The attribute loader of SPEAR-JSON Fig. 2 (block 3).
  *
  * Takes one descriptor at a time, reads the words that hold the attribute from
  * the InputBuffer and writes them, whole, to the FIFO of one parser channel.
  * Each word is tagged with the bytes that belong to the attribute. For every
  * attribute it also writes the attribute id to the reorder FIFO, so the output
  * side knows the order of the document. When the PDA signals the end of the
  * record, one end-of-record entry follows the entries of that record's
  * attributes.
  *
  * Channels are laid out as config.channelTypes. Attributes of one value type
  * are dealt to the channels of that type in rotation.
  *
  * Descriptors must have a nonzero length and stay within the 4096-byte buffer.
  * The memory contents they point at must stay unchanged until the attribute's
  * last word has been sent.
  */
class AttributeLoader(config: JsonPathConfig) extends Module {
  private val channelTypes = config.channelTypes
  private val perType = config.parsersPerType
  private val pointerWidth = log2Ceil(perType max 2)

  val io = IO(new Bundle {
    val descriptor = Flipped(Decoupled(new AttributeDescriptor))
    val channels = Vec(channelTypes.length, Decoupled(new AttributeWord))
    val reorder = Decoupled(new ReorderEntry)

    // The PDA's end-of-record pulse. It never comes before the last descriptor
    // of its record has been accepted.
    val endOfRecord = Input(Bool())
    // An end of record has been signaled but its reorder entry is not written.
    val endPending = Output(Bool())

    // Read port of the InputBuffer. The loader is its only user.
    val readEnable = Output(Bool())
    val readAddress = Output(UInt(9.W))
    val readData = Input(UInt(64.W))
    val readValid = Input(Bool())
  })

  val idle :: request :: waitForWord :: emit :: Nil = Enum(4)
  val state = RegInit(idle)
  val address = Reg(UInt(12.W)) // of the next attribute byte to send
  val remaining = Reg(UInt(13.W)) // attribute bytes not sent yet
  val attributeId = Reg(UInt(8.W))
  val channel = Reg(UInt(log2Ceil(channelTypes.length max 2).W))
  val word = Reg(UInt(64.W))

  // One pointer per value type: the next channel of that type to use.
  val pointers = RegInit(
    VecInit(Seq.fill(config.usedValueTypes.length)(0.U(pointerWidth.W)))
  )
  // isType(k): the descriptor belongs to a field of value type k.
  val id = io.descriptor.bits.attributeId
  val isType = config.usedValueTypes.map { valueType =>
    config.fields.zipWithIndex
      .collect { case (field, i) if field.valueType == valueType => id === i.U }
      .reduce(_ || _)
  }
  // The channels of type k are k * perType up to k * perType + perType - 1.
  val target = Mux1H(
    isType,
    pointers.zipWithIndex.map { case (pointer, k) =>
      (k * perType).U +& pointer
    }
  )

  // The end-of-record pulse cannot be stalled, so it is remembered here until
  // its entry fits in the reorder FIFO. No descriptor is accepted in the
  // meantime, so the entry always comes before those of the next record.
  val endPending = RegInit(false.B)
  when(endPending && io.reorder.ready) { endPending := false.B }
  when(io.endOfRecord) { endPending := true.B }
  io.endPending := endPending

  // The reorder entry of an attribute is written in the cycle its descriptor
  // is accepted, so one must fit.
  io.descriptor.ready := state === idle && io.reorder.ready && !endPending
  io.reorder.valid := endPending || (state === idle && io.descriptor.valid)
  io.reorder.bits.attributeId := id // not used by an end-of-record entry
  io.reorder.bits.endOfRecord := endPending

  io.readEnable := state === request
  io.readAddress := address(11, 3)

  val firstByte = address(2, 0)
  val room = 8.U(4.W) - firstByte // bytes from firstByte to the end of the word
  val isLastWord = remaining <= room
  val bytesInWord = Mux(isLastWord, remaining(3, 0), room)

  for ((out, i) <- io.channels.zipWithIndex) {
    out.valid := state === emit && channel === i.U
    out.bits.data := word
    out.bits.attributeId := attributeId
    out.bits.firstByte := firstByte
    out.bits.lastByte := firstByte + bytesInWord - 1.U
    out.bits.last := isLastWord
  }
  val wordSent = io.channels.map(_.fire).reduce(_ || _)

  switch(state) {
    is(idle) {
      when(io.descriptor.fire) {
        address := io.descriptor.bits.startAddress
        remaining := io.descriptor.bits.length
        attributeId := id
        channel := target
        // Move the pointer of this value type to its next channel.
        for ((pointer, k) <- pointers.zipWithIndex) {
          when(isType(k)) {
            pointer := Mux(pointer === (perType - 1).U, 0.U, pointer + 1.U)
          }
        }
        state := request
      }
    }
    is(request) {
      state := waitForWord
    }
    is(waitForWord) {
      when(io.readValid) {
        word := io.readData
        state := emit
      }
    }
    is(emit) {
      when(wordSent) {
        when(isLastWord) {
          state := idle
        }.otherwise {
          address := address + bytesInWord // the start of the next word
          remaining := remaining - bytesInWord
          state := request
        }
      }
    }
  }
}
