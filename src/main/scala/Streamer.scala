import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Queue

class Streamer() extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(8.W)))
    val out = Decoupled(Vec(8, UInt(8.W)))
  })
  val count = RegInit(0.U(3.W))
  val currentWord = RegInit(VecInit(Seq.fill(8)(0.U(8.W))))
  val full = RegInit(false.B)
  val padding = RegInit(false.B)
  val queue = Module(new Queue(Vec(8, UInt(8.W)), entries = 8))

  when(padding) {
    currentWord(count) := 32.U
    when(count === 7.U) {
      full := true.B
      padding := false.B
    }.elsewhen(count < 7.U) {
      count := count + 1.U
    }
  }.elsewhen(io.in.fire) {
    when(io.in.bits === 10.U) { // meaning the 0x0A: newline character.
      currentWord(count) := io.in.bits
      when(count === 7.U) {
        padding := false.B
        full := true.B
      }.otherwise {
        count := count + 1.U
        padding := true.B
      }
    }.otherwise {
      currentWord(count) := io.in.bits
      when(count === 7.U) {
        full := true.B
        padding := false.B
      }.elsewhen(count < 7.U) {
        count := count + 1.U
      }
    }
  }

  when(queue.io.enq.fire) {
    full := false.B
    count := 0.U
  }

  io.in.ready := !full && !padding
  queue.io.enq.bits := currentWord
  queue.io.enq.valid := full

  io.out <> queue.io.deq
}
