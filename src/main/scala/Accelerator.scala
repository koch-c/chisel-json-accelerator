import chisel3._
import chisel3.util._

/** Streaming, newline-delimited scalar-value JSON extractor.
  * Each complete line (including LF) must fit in 4096 bytes. Send one record
  * at a time and wait for processing/TX to finish; UART has no flow control.
  * TX contains exactly the AttributeLoader bytes, without added framing.
  */
class Accelerator(
    config: JsonPathConfig = JsonPathConfig(Seq("temperature")),
    clockHz: Int = 100000000,
    baudRate: Int = 115200
) extends Module {
  val io = IO(new Bundle {
    val uartRx = Input(Bool())
    val uartTx = Output(Bool())
  })

  val clearRecord = RegInit(false.B)
  val recordReset = reset.asBool || clearRecord

  val receiver = Module(new UartRx(clockHz, baudRate))
  val transmitter = Module(new UartTx(clockHz, baudRate))
  receiver.io.rx := io.uartRx
  io.uartTx := transmitter.io.tx

  // Reset only the record-processing pipeline, never the UART.
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

  // Do not accept another line until the current record has drained.
  val inputClosed = RegInit(false.B)
  inputFIFO.io.in.valid := receiver.io.out.valid && !inputClosed && !clearRecord
  inputFIFO.io.in.bits := receiver.io.out.bits
  receiver.io.out.ready := inputFIFO.io.in.ready && !inputClosed && !clearRecord
  when(inputFIFO.io.in.fire && inputFIFO.io.in.bits === 10.U) {
    inputClosed := true.B
  }

  // Atomically write each word to memory and hand it to the lexer. The lexer
  // sees a word only after its bytes have been stored, so later descriptors
  // cannot refer to unwritten data. Memory has no write-side backpressure.
  val wordCount = RegInit(0.U(10.W))
  val lineStored = RegInit(false.B)
  val canStore = wordCount < 512.U && !clearRecord
  lexer.io.in.valid := inputFIFO.io.out.valid && canStore
  lexer.io.in.bits := inputFIFO.io.out.bits
  inputFIFO.io.out.ready := lexer.io.in.ready && canStore
  buffer.io.writeEnable := inputFIFO.io.out.fire
  buffer.io.writeAddress := wordCount(8, 0)
  buffer.io.writeData := inputFIFO.io.out.bits.asUInt
  when(inputFIFO.io.out.fire) {
    wordCount := wordCount + 1.U
    when(inputFIFO.io.out.bits.map(_ === 10.U).reduce(_ || _)) {
      lineStored := true.B
    }
  }
  // Oversized input is outside the MVP contract; never wrap and overwrite RAM.
  when(inputFIFO.io.out.valid && !clearRecord) {
    assert(wordCount < 512.U, "JSON line exceeds the 4096-byte InputBuffer")
  }

  // Only AttributeLoader reads memory; no replay or read-port arbitration.
  buffer.io.readEnable := loader.io.readEnable
  buffer.io.readAddress := loader.io.readAddress
  loader.io.readData := buffer.io.readData
  loader.io.readValid := buffer.io.readValid

  // Lexer currently emits one symbol at a time; adapt it to a packed batch.
  fifo.io.in.valid := lexer.io.out.valid
  fifo.io.in.bits.count := 1.U
  fifo.io.in.bits.entries(0) := lexer.io.out.bits
  lexer.io.out.ready := fifo.io.in.ready
  pda.io.in <> fifo.io.out
  loader.io.descriptor <> pda.io.descriptor

  transmitter.io.in.valid := loader.io.out.valid
  transmitter.io.in.bits := loader.io.out.bits.data
  loader.io.out.ready := transmitter.io.in.ready

  val endSeen = RegInit(false.B)
  val attributeBusy = RegInit(false.B)
  when(pda.io.endOfRecord) { endSeen := true.B }
  when(loader.io.descriptor.fire) { attributeBusy := true.B }
  when(loader.io.out.fire && loader.io.out.bits.last) { attributeBusy := false.B }

  // The closing brace can be processed before LF arrives. Wait for both the
  // line terminator and parsing/output completion before resetting the record.
  clearRecord := false.B
  when(!clearRecord && lineStored && endSeen && !attributeBusy && transmitter.io.in.ready) {
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

object GenerateAccelerator extends App {
  emitVerilog(
    new Accelerator(JsonPathConfig(Seq("temperature"))),
    Array("--target-dir", "generated")
  )
}
