import chisel3._

class Debouncer(count: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Bool())
    val fire = Output(Bool())
  })
  val is_pressed = RegInit(false.B)
  is_pressed := io.in
  val start_pressing = !is_pressed && io.in
  val counter_width = (math.ceil(math.log(count) / math.log(2))).toInt.W
  val counter = RegInit(0.U(counter_width))
  counter := Mux(start_pressing, 0.U, counter + 1.U)

  io.fire := counter === count.U && is_pressed
}
