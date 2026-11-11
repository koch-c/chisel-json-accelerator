import chisel3._
import chisel3.util._

/** Streaming scalar-value JSON extractor with byte-level ready/valid ports.
  * Each valid JSON object line, including LF, must fit within 4096 bytes.
  * out contains the exact AttributeLoader bytes, without added framing.
  * done pulses for one cycle after LF, parsing, and all output transfers finish.
  * It denotes byte acceptance by the consumer, not physical UART completion.
  */
class Accelerator(config: JsonPathConfig = JsonPathConfig(Seq("temperature"))) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(8.W)))
    val out = Decoupled(UInt(8.W))
    val done = Output(Bool()) //TODO: Only used for testing. Consider removing in prod
  })

  val clearRecord = RegInit(false.B)
  val recordReset = reset.asBool || clearRecord
  io.done := clearRecord

  val inputFIFO = withReset(recordReset) { Module(new InputFIFO) }
  val lexer = withReset(recordReset) {
    Module(new Lexer(config.path.distinct, maxDepth = config.maxDepth max 2, addrWidth = 12))
  }
  val fifo = withReset(recordReset) {
    Module(new ChannelFifo(new JsonSymbol(config.keyIds.size, 12),
      depth = 16, enqueueWidth = 1, dequeueWidth = 2))
  }
  val pda = withReset(recordReset) { Module(new PushdownAutomaton(config)) }
  val loader = withReset(recordReset) { Module(new AttributeLoader) }
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

  // Adapt the lexer's single-symbol output to the FIFO's packed interface.
  // TODO: the lexer should be parallel and emit 8 symbols at a time.
  fifo.io.in.valid := lexer.io.out.valid
  fifo.io.in.bits.count := 1.U
  fifo.io.in.bits.entries(0) := lexer.io.out.bits
  lexer.io.out.ready := fifo.io.in.ready
  pda.io.in <> fifo.io.out
  loader.io.descriptor <> pda.io.descriptor

  io.out.valid := loader.io.out.valid
  io.out.bits := loader.io.out.bits.data
  loader.io.out.ready := io.out.ready

  val endSeen = RegInit(false.B)
  val attributeBusy = RegInit(false.B)
  when(pda.io.endOfRecord) { endSeen := true.B }
  when(loader.io.descriptor.fire) { attributeBusy := true.B }
  when(loader.io.out.fire && loader.io.out.bits.last) { attributeBusy := false.B }

  // A closing brace may be parsed before LF arrives. Output backpressure holds
  // attributeBusy until the final byte is actually accepted by the consumer.
  clearRecord := false.B
  when(!clearRecord && lineStored && endSeen && !attributeBusy) {
    clearRecord := true.B
  }
  when(clearRecord) {
    inputClosed := false.B
    wordCount := 0.U
    lineStored := false.B
    endSeen := false.B
    attributeBusy := false.B
  }
}
