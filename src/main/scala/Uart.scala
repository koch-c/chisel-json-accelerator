import chisel3._

/** Echoes received 8N1 UART bytes back to the sender. */
class Uart(clockHz: Int, baudRate: Int) extends Module {
  val io = IO(new Bundle {
    val uartRx = Input(Bool())
    val uartTx = Output(Bool())
  })

  val receiver = Module(new UartRx(clockHz, baudRate))
  val transmitter = Module(new UartTx(clockHz, baudRate))

  receiver.io.rx := io.uartRx
  transmitter.io.in <> receiver.io.out
  io.uartTx := transmitter.io.tx
}

object GenerateUart extends App {
  emitVerilog(
    new Uart(100_000_000, 115_200),
    Array("--target-dir", "generated")
  )
}
