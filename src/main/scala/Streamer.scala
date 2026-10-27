import chisel3._
import chisel3.util.Decoupled


class Streamer() extends Module {
    val io = IO(new Bundle {
        val in = Flipped(Decoupled(UInt(8.W)))
        val out = Decoupled(Vec(8, UInt(8.W)))
    })  
    val count = RegInit(0.U(3.W))
    val currentWord = RegInit(VecInit(Seq.fill(8)(0.U(8.W))))
    val full = RegInit(false.B)
    val padding = RegInit(false.B)
    

    when(padding){
      currentWord(count) := 32.U
      when(count === 7.U){
          full := true.B
          padding := false.B
        } .elsewhen(count < 7.U) {
          count := count + 1.U
        }
    }.elsewhen(io.in.fire){
      when(io.in.bits === 10.U){ // meaning the 0x0A: newline character.
        when(count =/= 0.U){
          padding := true.B
        }
      } .otherwise{
        currentWord(count) := io.in.bits
        when(count === 7.U){
          full := true.B
          padding := false.B
        } .elsewhen(count < 7.U) {
          count := count + 1.U
        }
      }
    }

    when(io.out.fire){
      full := false.B
      count := 0.U
    }


    io.in.ready := !full && !padding
    io.out.bits := currentWord
    io.out.valid := full
}
