import chisel3._
import chisel3.util.Decoupled
import chisel3.util.log2Ceil


class Streamer() extends Module {
    val io = IO(new Bundle {
        val out = Decoupled(Vec(8, UInt(8.W)))
    })  
    val count = RegInit(0.U(4.W))
    val currentWord = RegInit(VecInit(Seq.fill(8)(0.U(8.W))))

    when()

}
