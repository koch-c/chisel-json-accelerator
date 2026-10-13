import chisel3._
import chisel3.util.Decoupled

class Accelerator(key: String, json: String) extends Module {
  val io = IO(new Bundle {
    val button = Input(Bool())
    val found = Output(Bool())
    val LED16_R = Output(Bool())
    val LED16_G = Output(Bool())
    val LED16_B = Output(Bool())
  })

  val parser = Module(new Parser(key))
  val streamer = Module(new Streamer(json))
  val debouncer = Module(new Debouncer(2_000_000))

  parser.io.in <> streamer.io.out
  debouncer.io.in := io.button

  // LED are on when pulled low. I think :)
  io.found := parser.io.found
  io.LED16_R := io.found
  io.LED16_G := !io.found
  io.LED16_B := true.B
}

object GenerateAccelerator extends App {
  emitVerilog(
    new Accelerator("temperature","{ \"temperature\": 25, \"humidity\": 60 }"),
    Array("--target-dir", "generated")
  )
}
