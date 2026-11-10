import chisel3._
import chisel3.util._

/** Byte-to-eight-byte width-converting FIFO.
  * LF is retained and immediately completes a word; unused lanes are spaces.
  * Without LF, partial words wait for more bytes. Lane 0 is the earliest byte.
  * Accepts one byte per cycle, including consecutive word boundaries, whenever
  * output capacity is available. depthWords excludes the partial-word register.
  */
class InputFIFO(depthWords: Int = 8) extends Module {
  require(depthWords > 0, "Input FIFO must hold at least one word")

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(8.W)))
    val out = Decoupled(Vec(8, UInt(8.W)))
  })

  val words = Module(new Queue(Vec(8, UInt(8.W)), depthWords))
  val partial = Reg(Vec(8, UInt(8.W)))
  val count = RegInit(0.U(3.W))
  val completesWord = count === 7.U || io.in.bits === 10.U

  // A completing byte and its word are accepted atomically. Earlier bytes can
  // still accumulate in the partial register even while the word queue is full.
  io.in.ready := !completesWord || words.io.enq.ready
  words.io.enq.valid := io.in.valid && completesWord
  for (i <- 0 until 8) {
    words.io.enq.bits(i) := Mux(i.U < count, partial(i),
      Mux(i.U === count, io.in.bits, 32.U(8.W)))
  }
  when(io.in.fire) {
    when(completesWord) {
      count := 0.U
    }.otherwise {
      partial(count) := io.in.bits
      count := count + 1.U
    }
  }
  io.out <> words.io.deq
}
