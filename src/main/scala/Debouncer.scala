import chisel3._
import chisel3.util.log2Ceil

class Debouncer(count: Int) extends Module {
  require(count > 0, "count must be positive")
  val io = IO(new Bundle {
    val in = Input(Bool())
    val fire = Output(Bool())
  })
  val is_pressed = RegInit(false.B)
  is_pressed := RegNext(RegNext(io.in)) // Synchronize the asynchronize button

  val start_pressing = !is_pressed && io.in
  val counter = RegInit(0.U(log2Ceil(count).W))
  counter := Mux(start_pressing, 0.U, counter + 1.U)

  io.fire := counter === (count.U - 1.U) && is_pressed
}
