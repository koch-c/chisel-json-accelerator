import chisel3.*
import chisel3.util.Decoupled

/** Accepts vectors of eight input bytes for JSON parsing. */
class Parser extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(Vec(8, UInt(8.W))))
  })

  io.in.ready := true.B
}
