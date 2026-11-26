import chisel3._
import chisel3.util._

/** One parser channel (SPEAR-JSON Fig. 2, block 4): a FIFO of attribute words,
  * the unpacker that turns them into bytes, and the parser for one value type.
  * Every channel has the same ports whatever its type.
  *
  * Raw sends the attribute bytes as they are. Number, Int32 and Int64 go
  * through NumberParser and NumberSerializer. String goes through StringParser.
  */
class ParserChannel(valueType: ValueType, fifoDepth: Int = 4) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AttributeWord))
    val out = Decoupled(UInt(8.W))
    val last = Output(Bool()) // out carries the final byte of the attribute
  })

  val unpacker = Module(new WordUnpacker)
  unpacker.io.in <> Queue(io.in, fifoDepth)
  val bytes = unpacker.io.out

  io.last := (valueType match {
    case ValueType.Raw =>
      io.out.valid := bytes.valid
      io.out.bits := bytes.bits.data
      bytes.ready := io.out.ready
      bytes.bits.last
    case ValueType.String =>
      val parser = Module(new StringParser)
      parser.io.in <> bytes
      io.out <> parser.io.out
      parser.io.last
    case _ =>
      val parser = Module(new NumberParser)
      val serializer = Module(new NumberSerializer(valueType))
      parser.io.in <> bytes
      serializer.io.in <> parser.io.out
      io.out <> serializer.io.out
      serializer.io.last
  })
}
