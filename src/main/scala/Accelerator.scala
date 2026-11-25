import chisel3._
import chisel3.util._

/** Streaming scalar-value JSON extractor with byte-level ready/valid ports.
  * Each valid JSON object line, including LF, must fit within 4096 bytes. out
  * depends on the value type of the matched path: the exact bytes of the
  * attribute without added framing (Raw), a NumberSerializer frame, or a
  * StringParser frame. Values are sent in document order. done pulses for one
  * cycle after LF, parsing, and all output transfers finish. It denotes byte
  * acceptance by the consumer, not physical UART completion.
  */
class Accelerator(
    config: JsonPathConfig = JsonPathConfig(Seq(FieldSpec(Seq("temperature"))))
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
        dequeueWidth = 2
      )
    )
  }
  val pda = withReset(recordReset) { Module(new PushdownAutomaton(config)) }
  val execution = withReset(recordReset) { Module(new ExecutionStage(config)) }
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
  buffer.io.readEnable := execution.io.readEnable
  buffer.io.readAddress := execution.io.readAddress
  execution.io.readData := buffer.io.readData
  execution.io.readValid := buffer.io.readValid

  fifo.io.in <> lexer.io.out
  pda.io.in <> fifo.io.out
  execution.io.descriptor <> pda.io.descriptor

  // The execution stage loads each value and sends it raw, as a number or as
  // a string, depending on the value type of its path.
  io.out <> execution.io.out

  val endSeen = RegInit(false.B)
  when(pda.io.endOfRecord) { endSeen := true.B }

  // A closing brace may be parsed before LF arrives. The execution stage is
  // busy from the moment it accepts a descriptor until the final byte of that
  // attribute is actually accepted by the consumer, so output backpressure
  // delays the reset.
  clearRecord := false.B
  when(!clearRecord && lineStored && endSeen && !execution.io.busy) {
    clearRecord := true.B
  }
  when(clearRecord) {
    inputClosed := false.B
    wordCount := 0.U
    lineStored := false.B
    endSeen := false.B
  }
}
