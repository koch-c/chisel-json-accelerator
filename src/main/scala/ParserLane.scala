import chisel3._
import chisel3.util._

/** One attribute pipeline: an AttributeLoader followed by the parser for one
  * value type. Every lane has the same ports whatever its type, so lanes of
  * different types can sit side by side behind one dispatcher.
  *
  * Raw sends the loader bytes as they are. Number, Int32 and Int64 go through
  * NumberParser and NumberSerializer. String goes through StringParser.
  */
class ParserLane(valueType: ValueType) extends Module {
  val io = IO(new Bundle {
    val descriptor = Flipped(Decoupled(new AttributeDescriptor))
    val out = Decoupled(UInt(8.W))
    val last = Output(Bool()) // out carries the final byte of the attribute

    // Read port towards the InputBuffer, see AttributeLoader.
    val read = Decoupled(UInt(9.W))
    val readData = Input(UInt(64.W))
    val readValid = Input(Bool())
  })

  val loader = Module(new AttributeLoader)
  loader.io.descriptor <> io.descriptor
  io.read <> loader.io.read
  loader.io.readData := io.readData
  loader.io.readValid := io.readValid

  io.last := (valueType match {
    case ValueType.Raw =>
      io.out.valid := loader.io.out.valid
      io.out.bits := loader.io.out.bits.data
      loader.io.out.ready := io.out.ready
      loader.io.out.bits.last
    case ValueType.String =>
      val parser = Module(new StringParser)
      parser.io.in <> loader.io.out
      io.out <> parser.io.out
      parser.io.last
    case _ =>
      val parser = Module(new NumberParser)
      val serializer = Module(new NumberSerializer(valueType))
      parser.io.in <> loader.io.out
      serializer.io.in <> parser.io.out
      io.out <> serializer.io.out
      serializer.io.last
  })
}
