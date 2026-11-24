import chisel3._
import chisel3.util._

// Glue between the execution stage as it exists today (PushdownAutomaton,
// AttributeLoader, NumberParser, StringParser) and the OutputStage. None of
// those modules has to change. Wiring, with the modules in this file marked *:
//
//   pda.descriptor --> *ReorderWriter --> loader.descriptor
//   pda.endOfRecord -> *ReorderWriter.endOfRecord
//                      *ReorderWriter.reorder ----------------------> out.reorder
//   loader.out -> *AttributeRouter -+-> StringParser -> *StringFrameToBeats -> out.strings
//                                   +-> NumberParser -> *NumberToValue -+---> out.ints
//                                                                       +---> out.floats
//
// Attribute ids must be indices into OutputLayout.attributes, so the PDA has to
// give every path its own id.

/** Part 3's reorder output (SPEAR-JSON Fig. 2). Sits on the PDA to
  * AttributeLoader descriptor link: every descriptor the loader accepts writes
  * its attribute id, and the PDA's end-of-record pulse writes one endOfRecord
  * entry after them.
  *
  * The reorder entries outlive the record, so this module must be outside any
  * per-record reset. busy is high while an end of record has not been written.
  */
class ReorderWriter(depth: Int = 16) extends Module {
  require(depth >= 1, "the reorder FIFO must hold at least one entry")

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AttributeDescriptor)) // From the PDA.
    val out = Decoupled(new AttributeDescriptor) // To the AttributeLoader.
    val endOfRecord = Input(Bool()) // The PDA's endOfRecord pulse.
    val reorder = Decoupled(new ReorderEntry)
    val busy = Output(Bool())
  })

  val fifo = Module(new Queue(new ReorderEntry, depth))
  io.reorder <> fifo.io.deq

  // The PDA may pulse endOfRecord in the same cycle as the record's last
  // descriptor, and the pulse cannot be stalled. It is remembered here and
  // written in a later cycle. No descriptor passes while one is pending, so
  // the end of record always follows the descriptors of its record.
  val endPending = RegInit(false.B)
  val pass = !endPending && fifo.io.enq.ready

  io.out.valid := io.in.valid && pass
  io.out.bits := io.in.bits
  io.in.ready := io.out.ready && pass

  fifo.io.enq.valid := io.in.fire || endPending
  fifo.io.enq.bits.attributeId := io.in.bits.attributeId
  fifo.io.enq.bits.endOfRecord := endPending

  when(endPending && fifo.io.enq.ready) { endPending := false.B }
  when(io.endOfRecord) { endPending := true.B }
  io.busy := endPending
}

/** The type dispatch of part 3: the AttributeLoader has a single output, and
  * this sends each attribute's bytes to the parser for its type in the layout.
  * Bytes of an id that is not in the layout are dropped; the OutputStage drops
  * their reorder entries too.
  */
class AttributeRouter(layout: OutputLayout) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AttributeByte))
    val strings = Decoupled(new AttributeByte)
    val numbers = Decoupled(new AttributeByte)
  })

  private def idIs(matches: AttributeType => Boolean): Bool =
    layout.attributes.zipWithIndex
      .collect {
        case (attribute, id) if matches(attribute) =>
          io.in.bits.attributeId === id.U
      }
      .foldLeft(false.B)(_ || _)

  val isString = idIs(_.isInstanceOf[StringAttr])
  val isNumber = idIs(a => a.isInstanceOf[IntAttr] || a.isInstanceOf[FloatAttr])

  io.strings.valid := io.in.valid && isString
  io.numbers.valid := io.in.valid && isNumber
  io.strings.bits := io.in.bits
  io.numbers.bits := io.in.bits
  io.in.ready := Mux(
    isString,
    io.strings.ready,
    Mux(isNumber, io.numbers.ready, true.B)
  )
}

/** Turns a NumberParser result into the value its attribute is declared as.
  *
  * int: ok when the number is a JSON integer (no fraction or exponent) that
  * fits 64 bits; the OutputStage narrows it for int(32).
  *
  * float: TODO. The paper's "float norm" (decimal mantissa and exp10 to
  * IEEE-754) is not built yet, so every float is sent with ok = false. That
  * keeps the FIFOs aligned and marks the attribute absent.
  *
  * A result whose id is not a number attribute in the layout is dropped.
  */
class NumberToValue(layout: OutputLayout) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new ParsedNumber))
    val ints = Decoupled(new IntValue)
    val floats = Decoupled(new FloatValue)
  })

  private def idIs(matches: AttributeType => Boolean): Bool =
    layout.attributes.zipWithIndex
      .collect {
        case (attribute, id) if matches(attribute) =>
          io.in.bits.attributeId === id.U
      }
      .foldLeft(false.B)(_ || _)

  val isInt = idIs(_.isInstanceOf[IntAttr])
  val isFloat = idIs(_.isInstanceOf[FloatAttr])

  val n = io.in.bits
  val limit = (BigInt(1) << 63).U(64.W)
  val fits = Mux(n.negative, n.integer <= limit, n.integer < limit)
  io.ints.bits.value := Mux(n.negative, 0.U - n.integer, n.integer).asSInt
  io.ints.bits.ok := !n.notANumber && !n.overflow && !n.isFloat && fits

  io.floats.bits.bits := 0.U
  io.floats.bits.ok := false.B

  io.ints.valid := io.in.valid && isInt
  io.floats.valid := io.in.valid && isFloat
  io.in.ready := Mux(
    isInt,
    io.ints.ready,
    Mux(isFloat, io.floats.ready, true.B)
  )
}

/** Turns a StringParser frame into StringBeats.
  *
  * Frame: attribute id, the decoded bytes, 0x00, flags (bit 0 notAString, bit 1
  * badEscape), with last on the flags byte. A decoded byte is never 0x00.
  *
  * Whether a byte is the string's final one is only known when the terminator
  * arrives, and whether the string is usable only with the flags, so one byte
  * is held back and sent with the flags as the last beat.
  */
class StringFrameToBeats extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(8.W)))
    val inLast = Input(Bool()) // in carries the flags byte.
    val out = Decoupled(new StringBeat)
  })

  val header = RegInit(true.B) // The next byte is the attribute id.
  val held = RegInit(false.B)
  val heldData = Reg(UInt(8.W))

  val isTerminator = io.in.bits === 0.U && !io.inLast
  // The flags byte sends the last beat; a new data byte sends the held one.
  val emits = !header && (io.inLast || (!isTerminator && held))

  io.out.valid := io.in.valid && emits
  io.out.bits.data := heldData
  io.out.bits.empty := !held
  io.out.bits.last := io.inLast
  io.out.bits.ok := !io.inLast || io.in.bits(1, 0) === 0.U
  io.in.ready := !emits || io.out.ready

  when(io.in.fire) {
    when(header) {
      header := false.B
    }.elsewhen(io.inLast) {
      header := true.B
      held := false.B
    }.elsewhen(!isTerminator) {
      held := true.B
      heldData := io.in.bits
    }
  }
}
