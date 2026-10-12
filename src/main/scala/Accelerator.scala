import chisel3._
import chisel3.util.Decoupled

class Accelerator(key: String) extends Module {
  val io = IO(new Bundle {
    val found = Output(Bool())
    val LED16_R = Output(Bool())
    val LED16_G = Output(Bool())
    val LED16_B = Output(Bool())
  })

  val parser = Module(new Parser(key))
  val streamer = Module(new Streamer())
  parser.io.in <> streamer.io.out

  // LED are on when pulled low. I think :)
  io.found := parser.io.found
  io.LED16_R := io.found
  io.LED16_G := !io.found
  io.LED16_B := true.B
}

object GenerateAccelerator extends App {
  emitVerilog(
    new Accelerator("temperature"),
    Array("--target-dir", "generated")
  )
}
