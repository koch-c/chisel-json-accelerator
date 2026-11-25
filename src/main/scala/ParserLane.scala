import chisel3._
import chisel3.util._

// Unpacker module divides the word up so it can be handed to the parser.

class WordUnpacker() extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AttributeWord))
    val out = Decoupled(new AttributeByte)
  })
  val midWord = RegInit(false.B)
  val counter = Reg(UInt(3.W))

  val byteIndex = Mux(midWord, counter, io.in.bits.firstByte)
  val atLastByte = byteIndex === io.in.bits.lastByte

  io.out.valid := io.in.valid
  io.out.bits.data := (io.in.bits.data >> (byteIndex << 3))(7, 0)
  io.out.bits.attributeId := io.in.bits.attributeId
  io.out.bits.last := atLastByte && io.in.bits.last
  io.in.ready := atLastByte && io.out.ready

  when(io.out.fire) {
    when(atLastByte) {
      midWord := false.B
    }.otherwise {
      midWord := true.B
      counter := byteIndex + 1.U
    }
  }
}

/** One attribute pipeline: an AttributeLoader followed by the parser for one
  * value type. Every byte has the same ports whatever its type, so bytes of
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
