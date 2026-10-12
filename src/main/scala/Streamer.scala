import chisel3._
import chisel3.util.Decoupled

/** Receives vectors of eight bytes using Chisel's ready/valid interface. */
class Streamer extends Module {
  val io = IO(new Bundle {
    val out = Decoupled(Vec(8, UInt(8.W)))
  })

  io.out.valid := false.B
  io.out.bits := VecInit(Seq.fill(8)(0.U(8.W)))
}
