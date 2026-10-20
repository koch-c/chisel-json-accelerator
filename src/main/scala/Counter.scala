import chisel3._
import chisel3.util.Decoupled
import chisel3.util.log2Ceil
import chisel3.util.switch

class Counter(fire_at: Int) extends Module {
  val io = IO(new Bundle {
    val tick = Input(Bool())
    val fire = Output(Bool())
  })

  val reg = RegInit(0.U(log2Ceil(fire_at).W))
  val fire = reg === fire_at.U
  when(fire) {
    reg := Mux(io.tick, 1.U, 0.U)
  }.otherwise {
    reg := reg + Mux(io.tick, 1.U, 0.U)
  }
  io.fire := fire
}
