import chisel3._
import chisel3.util.Decoupled


class Streamer() extends Module {
    val io = IO(new Bundle {
        val in = Flipped(Decoupled(UInt(8.W)))
        val out = Decoupled(Vec(8, UInt(8.W)))
    })  
    val count = RegInit(0.U(4.W))
    val currentWord = RegInit(VecInit(Seq.fill(8)(0.U(8.W))))
    val full = RegInit(false.B)


    when(io.in.fire){
      currentWord(count) := io.in.bits
      when(count === 7.U){
        full := true.B
      } .elsewhen(count < 7.U) {
       count := count + 1.U
      }
    }

    when(io.out.fire){
      full := false.B
      count := 0.U
    }


    io.in.ready := !full
    io.out.bits := currentWord
    io.out.valid := full
}
