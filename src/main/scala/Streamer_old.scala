import chisel3._
import chisel3.util.Decoupled
import chisel3.util.log2Ceil

/** Receives json string using Chisel's ready/valid interface. */
class Streamer_old(json: String) extends Module {
  val io = IO(new Bundle {
    
    val out = Decoupled(Vec(8, UInt(8.W)))
  })
  val done = RegInit(false.B)

  def ch(c: Char) = c.toInt.U(8.W)

  val padded = json.padTo((json.length + 7) / 8 * 8, ' ')

  val size = padded.length / 8

  val currWord = RegInit(0.U(log2Ceil(size).W))

  val wordsTranslated = VecInit((for (word <- padded.grouped(8)) yield VecInit(word.map(ch))).toSeq)

  when(io.out.fire){
    currWord := currWord + 1.U
    when(currWord === (size - 1).U(log2Ceil(size).W)){
      done := true.B
    }
  }

  io.out.valid := !done
  io.out.bits := wordsTranslated(currWord)
  

}
