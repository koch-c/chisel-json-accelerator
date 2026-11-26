import chisel3._
import chisel3.util._

// Unpacker module divides the word up so it can be handed to the parser.

class WordUnpacker() extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new ValueChunk))
    val out = Decoupled(new AttributeByte)
  })
  val midWord = RegInit(false.B)
  val counter = Reg(UInt(3.W))

  val byteIndex = Mux(midWord, counter, io.in.bits.firstByte)
  val atLastByte = byteIndex === io.in.bits.lastByte

  io.out.valid := io.in.valid
  io.out.bits.data := (io.in.bits.data >> (byteIndex << 3))(7, 0)
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
