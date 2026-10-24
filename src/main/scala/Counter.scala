import chisel3._
import chisel3.util.log2Ceil

class Counter(fire_at: Int) extends Module {
  require(fire_at > 0)
  val io = IO(new Bundle {
    val start = Input(Bool())
    val fire = Output(Bool())
  })

  val reg = RegInit(0.U(log2Ceil(fire_at).W))
  val isStarted = RegInit(false.B)

  reg := reg + 1.U
  val fire = isStarted && reg === (fire_at - 1).U
  when(io.start) {
    reg := 0.U
    isStarted := true.B
  }.elsewhen(fire) {
    isStarted := false.B
  }
  io.fire := fire
}
