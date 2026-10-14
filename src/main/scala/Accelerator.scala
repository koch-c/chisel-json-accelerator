import chisel3._
import chisel3.util.Decoupled

class Accelerator(key: String, json: String, debounce: Int) extends Module {
  val io = IO(new Bundle {
    val button = Input(Bool())
    val found = Output(Bool())
    val LED16_R = Output(Bool())
    val LED16_G = Output(Bool())
    val LED16_B = Output(Bool())
  })

  val parser = Module(new Parser(key))
  val streamer = Module(new Streamer(json))
  val debouncer = Module(new Debouncer(debounce))
  debouncer.io.in := io.button
  val start = RegInit(false.B)
  start := Mux(debouncer.io.fire, true.B, start)

  parser.io.in.bits := streamer.io.out.bits
  streamer.io.out.ready := Mux(start, parser.io.in.ready, false.B)
  parser.io.in.valid := streamer.io.out.valid

  // LED are on when pulled low. I think :)
  io.found := parser.io.found
  io.LED16_R := io.found
  io.LED16_G := !io.found
  io.LED16_B := true.B
}

object GenerateAccelerator extends App {
  emitVerilog(
    new Accelerator("temperature","{ \"temperature\": 25, \"humidity\": 60 }", 2_000_000),
    Array("--target-dir", "generated")
  )
}
