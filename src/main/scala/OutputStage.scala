import chisel3._
import chisel3.util._

object OutputStage {

  /** Smallest power of two that holds one whole frame. */
  def defaultFifoDepth(layout: OutputLayout): Int =
    1 << log2Ceil(layout.frameBytes)
}

/** Output stage (SPEAR-JSON Fig. 2, block 5).
  *
  * Reads attribute ids from the reorder FIFO, moves each value from its type's
  * FIFO into that attribute's registers, and on the end-of-record entry hands
  * the record to the out buffer. There the filter is applied and the record is
  * serialized as one fixed-length frame (see OutputLayout).
  *
  * recordDone pulses when the end-of-record entry is consumed, which is before
  * the consumer has taken the frame. The stage holds unsent output, so it must
  * live outside any per-record reset.
  */
class OutputStage(layout: OutputLayout, outFifoDepth: Int) extends Module {
  def this(layout: OutputLayout) =
    this(layout, OutputStage.defaultFifoDepth(layout))
  require(outFifoDepth > 0, "the out FIFO must hold at least one byte")

  private val attributes = layout.attributes
  private val numAttributes = attributes.size

  val io = IO(new Bundle {
    val reorder = Flipped(Decoupled(new ReorderEntry))
    val strings = Flipped(Decoupled(new StringBeat))
    val ints = Flipped(Decoupled(new IntValue))
    val floats = Flipped(Decoupled(new FloatValue))
    val out = Decoupled(UInt(8.W))
    val recordDone = Output(Bool()) // The record left the attribute registers.
    val recordDropped = Output(Bool()) // The filter rejected a record.
  })

  // 1. Attribute-specific registers, filled in arrival order.
  val fields = RegInit(VecInit(Seq.fill(layout.recordBytes)(0.U(8.W))))
  val present = RegInit(VecInit(Seq.fill(numAttributes)(false.B)))
  val truncated = RegInit(VecInit(Seq.fill(numAttributes)(false.B)))

  val idle :: takeString :: takeInt :: takeFloat :: commit :: Nil = Enum(5)
  val state = RegInit(idle)
  val current = Reg(UInt(8.W)) // Attribute being filled.
  private val maxStringBytes = attributes
    .collect { case StringAttr(bytes) => bytes }
    .maxOption
    .getOrElse(1)
  val written = Reg(
    UInt(log2Ceil(maxStringBytes + 1).W)
  ) // Saturates at maxStringBytes.

  private def entryHasType(matches: AttributeType => Boolean): Bool =
    attributes.zipWithIndex
      .collect {
        case (attribute, id) if matches(attribute) =>
          io.reorder.bits.attributeId === id.U
      }
      .foldLeft(false.B)(_ || _)

  io.reorder.ready := state === idle
  io.strings.ready := state === takeString
  io.ints.ready := state === takeInt
  io.floats.ready := state === takeFloat

  when(io.reorder.fire) {
    current := io.reorder.bits.attributeId
    written := 0.U
    // An id outside the layout has no value FIFO to read; the entry is dropped.
    when(io.reorder.bits.endOfRecord) {
      state := commit
    }.elsewhen(entryHasType(_.isInstanceOf[StringAttr])) {
      state := takeString
    }.elsewhen(entryHasType(_.isInstanceOf[IntAttr])) {
      state := takeInt
    }.elsewhen(entryHasType(_.isInstanceOf[FloatAttr])) {
      state := takeFloat
    }
  }
  when(io.strings.fire) {
    when(!io.strings.bits.empty && written =/= maxStringBytes.U) {
      written := written + 1.U
    }
    when(io.strings.bits.last) { state := idle }
  }
  when(io.ints.fire || io.floats.fire) { state := idle }

  // Each attribute only decodes writes to its own slice of the record.
  for ((attribute, id) <- attributes.zipWithIndex) {
    val offset = layout.offsets(id)
    val slice = (0 until attribute.bytes).map(i => fields(offset + i))
    val selected = current === id.U

    attribute match {
      case StringAttr(bytes) =>
        // A duplicate key replaces the earlier value, so start from zeros.
        when(
          io.reorder.fire && !io.reorder.bits.endOfRecord &&
            io.reorder.bits.attributeId === id.U
        ) {
          slice.foreach(_ := 0.U)
          present(id) := false.B
          truncated(id) := false.B
        }
        when(io.strings.fire && selected) {
          when(!io.strings.bits.empty) {
            when(written < bytes.U) {
              for (i <- 0 until bytes) {
                when(written === i.U) { slice(i) := io.strings.bits.data }
              }
            }.otherwise {
              truncated(id) := true.B
            }
          }
          when(io.strings.bits.last) {
            present(id) := io.strings.bits.ok
            // An unusable string is absent with a zero field, like a number.
            when(!io.strings.bits.ok) {
              slice.foreach(_ := 0.U)
              truncated(id) := false.B
            }
          }
        }

      case IntAttr(bits) =>
        when(io.ints.fire && selected) {
          val value = io.ints.bits.value.asUInt
          // The value fits int(32) when bits 63..31 all equal the sign.
          val fits =
            if (bits == 64) true.B else value(63, 31).andR || !value(63, 31).orR
          val ok = io.ints.bits.ok && fits
          slice.zipWithIndex.foreach { case (byte, i) =>
            byte := Mux(ok, value(8 * i + 7, 8 * i), 0.U)
          }
          present(id) := ok
        }

      case FloatAttr(_) =>
        when(io.floats.fire && selected) {
          val ok = io.floats.bits.ok
          slice.zipWithIndex.foreach { case (byte, i) =>
            byte := Mux(ok, io.floats.bits.bits(8 * i + 7, 8 * i), 0.U)
          }
          present(id) := ok
        }
    }
  }

  // 2. Out buffer: holds one record while it is filtered and serialized, so
  // the registers above are free for the next record.
  val outFields = Reg(Vec(layout.recordBytes, UInt(8.W)))
  val outPresent = Reg(Vec(numAttributes, Bool()))
  val outTruncated = Reg(Vec(numAttributes, Bool()))
  val outValid = RegInit(false.B)

  val committing = state === commit && !outValid
  io.recordDone := committing
  when(committing) {
    outFields := fields
    outPresent := present
    outTruncated := truncated
    outValid := true.B
    fields.foreach(_ := 0.U)
    present.foreach(_ := false.B)
    truncated.foreach(_ := false.B)
    state := idle
  }

  // 3. Selection, then serialization into the out FIFO.
  val outFifo = Module(new Queue(UInt(8.W), outFifoDepth))
  io.out <> outFifo.io.deq

  val keep = SelectionLogic.keep(layout, outFields, outPresent, outTruncated)
  private val presentMask = (0 until layout.presentBytes).map { byte =>
    Cat((7 to 0 by -1).map { bit =>
      val id = 8 * byte + bit
      if (id < numAttributes) outPresent(id) else false.B
    })
  }
  val frame = VecInit(presentMask ++ outFields)
  val sending = RegInit(false.B)
  val index = Reg(UInt(log2Ceil(layout.frameBytes).W))

  outFifo.io.enq.valid := sending
  outFifo.io.enq.bits := frame(index)
  io.recordDropped := false.B

  when(outValid && !sending) {
    when(keep) {
      sending := true.B
      index := 0.U
    }.otherwise {
      outValid := false.B
      io.recordDropped := true.B
    }
  }
  when(outFifo.io.enq.fire) {
    index := index + 1.U
    when(index === (layout.frameBytes - 1).U) {
      sending := false.B
      outValid := false.B
    }
  }
}
