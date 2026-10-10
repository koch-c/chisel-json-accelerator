import chisel3._
import chisel3.util.Decoupled

/** Receives vectors of eight bytes using Chisel's ready/valid interface. */
class Streamer extends Module {
  val io = IO(new Bundle {
    val in = Decoupled(Vec(8, UInt(8.W)))
  })

  // The streamer can accept a word on every cycle.
  io.in.ready := true.B
}


