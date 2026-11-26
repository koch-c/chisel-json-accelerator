import chisel3._
import chisel3.util._

/** Emits an outcome byte and, if kept, a fixed-size record per JSON line. */
class Accelerator(
    config: JsonPathConfig =
      JsonPathConfig(Seq(FieldSpec(Seq("id")))),
    pdaWidth: Int = 2
) extends Module {

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(8.W)))
    val out = Decoupled(UInt(8.W))
    val done =
      Output(Bool()) // TODO: Only used for testing. Consider removing in prod
  })

  val clearRecord = RegInit(false.B)
  val recordReset = reset.asBool || clearRecord
  io.done := clearRecord

  val inputFIFO = withReset(recordReset) { Module(new InputFIFO) }
  val lexer = withReset(recordReset) {
    Module(
      new Lexer(config.keys, maxDepth = config.maxDepth max 2, addrWidth = 12)
    )
  }
  val fifo = withReset(recordReset) {
    Module(
      new ChannelFifo(
        new JsonSymbol(config.keyIds.size, 12),
        depth = 32,
        enqueueWidth = 8,
        dequeueWidth = pdaWidth
      )
    )
  }
  val pda = withReset(recordReset) {
    Module(new PushdownAutomaton(config, pdaWidth))
  }
  val loader = withReset(recordReset) { Module(new AttributeLoader(config)) }
  val parserBank = withReset(recordReset) {
    Module(new ValueParserBank(config))
  }
  val buffer = Module(new InputBuffer)

  // Stop at LF until this record has drained and its pipeline is reset.
  val inputClosed = RegInit(false.B)
  inputFIFO.io.in.bits := io.in.bits
  inputFIFO.io.in.valid := io.in.valid && !inputClosed && !clearRecord
  io.in.ready := inputFIFO.io.in.ready && !inputClosed && !clearRecord
  when(inputFIFO.io.in.fire && inputFIFO.io.in.bits === 10.U) {
    inputClosed := true.B
  }

  // A word is written to RAM on the same edge that the lexer accepts it.
  // Later PDA descriptors therefore cannot refer to unwritten data.
  // Input is assumed to fit in the buffer. TODO: make buffer size configurable.
  val wordCount = RegInit(0.U(9.W))
  val lineStored = RegInit(false.B)
  lexer.io.in.valid := inputFIFO.io.out.valid && !clearRecord
  lexer.io.in.bits := inputFIFO.io.out.bits
  inputFIFO.io.out.ready := lexer.io.in.ready && !clearRecord
  buffer.io.writeEnable := inputFIFO.io.out.fire
  buffer.io.writeAddress := wordCount
  buffer.io.writeData := inputFIFO.io.out.bits.asUInt
  when(inputFIFO.io.out.fire) {
    wordCount := wordCount + 1.U
    when(inputFIFO.io.out.bits.map(_ === 10.U).reduce(_ || _)) {
      lineStored := true.B
    }
  }
  buffer.io.readEnable := loader.io.readEnable
  buffer.io.readAddress := loader.io.readAddress
  loader.io.readData := buffer.io.readData
  loader.io.readValid := buffer.io.readValid

  fifo.io.in <> lexer.io.out
  pda.io.in <> fifo.io.out
  loader.io.descriptor <> pda.io.descriptor
  loader.io.endOfRecord := pda.io.endOfRecord
  parserBank.io.channels <> loader.io.channels
  val reorder = withReset(recordReset) {
    Module(new Queue(new ResultOrderEntry(config), 16))
  }
  val assembler = withReset(recordReset) { Module(new RecordAssembler(config)) }
  val serializer = withReset(recordReset) {
    Module(new RecordSerializer(config.recordBytes))
  }
  reorder.io.enq <> loader.io.reorder
  assembler.io.reorder <> reorder.io.deq
  assembler.io.numbers <> parserBank.io.numbers
  assembler.io.strings <> parserBank.io.strings
  val filter = Module(new RecordFilter(config))
  serializer.io.record <> assembler.io.record
  filter.io.record := assembler.io.record.bits
  serializer.io.keep := filter.io.keep
  io.out <> serializer.io.out
  val endDrained = RegInit(false.B)
  when(assembler.io.record.fire) { endDrained := true.B }
  clearRecord := false.B
  when(!clearRecord && lineStored && endDrained) {
    clearRecord := true.B
  }
  when(clearRecord) {
    inputClosed := false.B
    wordCount := 0.U
    lineStored := false.B
    endDrained := false.B
  }
}
