import chisel3._
import chisel3.util._

/** Byte-to-eight-byte width-converting FIFO. LF is retained and immediately
  * completes a word; unused lanes are spaces. Without LF, partial words wait
  * for more bytes. Lane 0 is the earliest byte. Accepts one byte per cycle,
  * including consecutive word boundaries, whenever output capacity is
  * available. depthWords excludes the partial-word register.
  */
class InputFIFO(depthWords: Int = 8) extends Module {
  require(depthWords > 0, "Input FIFO must hold at least one word")

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Batch(8)))
    val out = Decoupled(Vec(8, UInt(8.W)))
  })

  val words = Module(new Queue(Vec(8, UInt(8.W)), depthWords))
  // The count is valid as long as the count is not zero and <= than eight.
  val countValid = io.in.bits.count =/= 0.U && io.in.bits.count <= 8.U

  // If the count is valid, we can accept the input.
  io.in.ready := words.io.enq.ready && countValid
  words.io.enq.valid := io.in.valid && countValid


  for (i <- 0 until 8) {
    words.io.enq.bits(i) := Mux(
      i.U < io.in.bits.count,
      io.in.bits.bytes(i),
      32.U(8.W)
    )
  }

  io.out <> words.io.deq
}
