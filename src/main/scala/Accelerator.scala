import chisel3._
import chisel3.util._

/** Streaming scalar-value JSON extractor with byte-level ready/valid ports.
  * Each valid JSON object line, including LF, must fit within 4096 bytes. out
  * depends on config.valueType: the exact AttributeLoader bytes without added
  * framing (Raw), a NumberSerializer frame, or a StringParser frame. done
  * pulses for one cycle after LF, parsing, and all output transfers finish. It
  * denotes byte acceptance by the consumer, not physical UART completion.
  */
class Accelerator(
    config: JsonPathConfig = JsonPathConfig(Seq(Seq("temperature")))
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

  fifo.io.in <> lexer.io.out
  pda.io.in <> fifo.io.out
  loader.io.descriptor <> pda.io.descriptor

  // Output stage can choose between the raw output, a parsed number or a decoded string.
  val lastOut = config.valueType match {
    case ValueType.Raw =>
      io.out.valid := loader.io.out.valid
      io.out.bits := loader.io.out.bits.data
      loader.io.out.ready := io.out.ready
      loader.io.out.bits.last
    case ValueType.String =>
      val parser = withReset(recordReset) { Module(new StringParser) }
      parser.io.in <> loader.io.out
      io.out <> parser.io.out
      parser.io.last
    case _ =>
      val parser = withReset(recordReset) { Module(new NumberParser) }
      val serializer = withReset(recordReset) {
        Module(new NumberSerializer(config.valueType))
      }
      parser.io.in <> loader.io.out
      serializer.io.in <> parser.io.out
      io.out <> serializer.io.out
      serializer.io.last
  }

  val endSeen = RegInit(false.B)

  // Attributes that have been handed to the loader but whose output has not
  // been fully accepted yet. At most three: one each in the loader, the number
  // parser and the serializer (the string path has only the loader and the
  // string parser).
  val pendingAttributes = RegInit(0.U(2.W))
  val attributeStarted = loader.io.descriptor.fire
  val attributeFinished = io.out.fire && lastOut
  val attributeBusy = pendingAttributes =/= 0.U

  when(pda.io.endOfRecord) { endSeen := true.B }
  when(attributeStarted && !attributeFinished) {
    pendingAttributes := pendingAttributes + 1.U
  }
  when(attributeFinished && !attributeStarted) {
    pendingAttributes := pendingAttributes - 1.U
  }

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
    pendingAttributes := 0.U
  }
}
