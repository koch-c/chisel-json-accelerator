import chisel3._
import chisel3.util._

class DispatchEntry(config: JsonPathConfig) extends ReorderEntry {
  val channel = UInt(log2Ceil(config.channelTypes.size max 2).W)
}

class TextBeat extends Bundle {
  val data = UInt(8.W) // terminal beat contains error flags instead of data
  val last = Bool()
}

/** Each channel buffers words and parses independently of document order. */
class ValueParserBank(config: JsonPathConfig) extends Module {
  val io = IO(new Bundle {
    val channels =
      Flipped(Vec(config.channelTypes.size, Decoupled(new AttributeWord)))
    val numbers = Vec(config.numbers, Decoupled(new ParsedNumber))
    val strings = Vec(config.strings, Decoupled(new TextBeat))
  })
  for (i <- config.channelTypes.indices) {
    val unpack = Module(new WordUnpacker)
    unpack.io.in <> Queue(io.channels(i), 4)
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
