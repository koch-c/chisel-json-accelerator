import chisel3._
import chisel3.util.log2Ceil

/** Parses one space-padded UART document per reset and reports the result. */
class Accelerator(key: String, clockHz: Int = 100_000_000, baudRate: Int = 115_200) extends Module {
  val io = IO(new Bundle {
    val uartRx = Input(Bool())
    val uartTx = Output(Bool())
    val LED16_R = Output(Bool())
    val LED16_G = Output(Bool())
    val LED16_B = Output(Bool())
  })

  val parser = Module(new Parser(key))
  val streamer = Module(new Streamer())
  val receiver = Module(new UartRx(clockHz, baudRate))
  val transmitter = Module(new UartTx(clockHz, baudRate))

  receiver.io.rx := io.uartRx
  streamer.io.in <> receiver.io.out
  parser.io.in <> streamer.io.out
  io.uartTx := transmitter.io.tx

  // Parser results remain latched until reset. Hold each response byte until
  // TX accepts it, and never restart the response while done stays high.
  val foundReply = "FOUND\n"
  val missingReply = "NOT_FOUND\n"
  val foundBytes = VecInit(foundReply.padTo(missingReply.length, ' ').map(_.toInt.U(8.W)))
  val missingBytes = VecInit(missingReply.map(_.toInt.U(8.W)))
  val replyIndex = RegInit(0.U(log2Ceil(missingReply.length).W))
  val replySent = RegInit(false.B)
  val replyLength = Mux(parser.io.found, foundReply.length.U, missingReply.length.U)

  transmitter.io.in.valid := parser.io.done && !replySent
  transmitter.io.in.bits := Mux(parser.io.found, foundBytes(replyIndex), missingBytes(replyIndex))
  when(transmitter.io.in.fire) {
    when(replyIndex === replyLength - 1.U) {
      replySent := true.B
    }.otherwise {
      replyIndex := replyIndex + 1.U
    }
  }

  val found = parser.io.found
  def is_done(bit: Bool) = parser.io.done && bit

  io.LED16_R := is_done(!found)
  io.LED16_G := is_done(found)
  io.LED16_B := false.B
}

object GenerateAccelerator extends App {
  emitVerilog(
    new Accelerator("temperature"),
    Array("--target-dir", "generated")
  )
}
