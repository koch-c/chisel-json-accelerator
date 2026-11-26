import chisel3._
import chisel3.util._

/** A matched field and its channel, or an end marker that has no result. */
class ResultOrderEntry(config: JsonPathConfig) extends Bundle {
  val attributeId = UInt(8.W)
  val endOfRecord = Bool()
  val channel = UInt(log2Ceil(config.channelCount max 2).W)
}

class TextBeat extends Bundle {
  val data = UInt(8.W) // terminal beat contains error flags instead of data
  val last = Bool()
}

/** Each channel buffers words and parses independently of document order. */
class ValueParserBank(config: JsonPathConfig) extends Module {
  val io = IO(new Bundle {
    val channels =
      Flipped(Vec(config.channelCount, Decoupled(new ValueChunk)))
    val numbers = Vec(config.numbers, Decoupled(new ParsedNumber))
    val strings = Vec(config.strings, Decoupled(new TextBeat))
  })
  for (i <- 0 until config.channelCount) {
    val unpack = Module(new WordUnpacker)
    val channelQueue = Module(new Queue(new ValueChunk, 4))
    val unpackStage = Module(new Queue(new ValueChunk, 2))
    channelQueue.io.enq <> io.channels(i)
    unpackStage.io.enq <> channelQueue.io.deq
    unpack.io.in <> unpackStage.io.deq
    if (i < config.numbers) {
      val parser = Module(new NumberParser)
      parser.io.in <> unpack.io.out
      io.numbers(i) <> parser.io.out
    } else {
      val parser = Module(new StringParser)
      parser.io.in <> unpack.io.out
      val port = io.strings(i - config.numbers)
      port.valid := parser.io.out.valid
      port.bits.data := parser.io.out.bits
      port.bits.last := parser.io.last
      parser.io.out.ready := port.ready
    }
  }
}
