import chisel3._

/** UART wrapper around the byte-stream extractor.
  * Send one JSON object line at a time; each line including LF must fit in
  * 4096 bytes. UART has no flow control. TX adds no framing to the raw output.
  */
class AcceleratorTop(
    config: JsonPathConfig = JsonPathConfig(Seq("temperature")),
    clockHz: Int = 100000000,
    baudRate: Int = 115200
) extends Module {
  val io = IO(new Bundle {
    val uartRx = Input(Bool())
    val uartTx = Output(Bool())
  })

  val receiver = Module(new UartRx(clockHz, baudRate))
  val core = Module(new Accelerator(config))
  val transmitter = Module(new UartTx(clockHz, baudRate))

  receiver.io.rx := io.uartRx
  core.io.in <> receiver.io.out
  transmitter.io.in <> core.io.out
  io.uartTx := transmitter.io.tx
}

object GenerateAcceleratorTop extends App {
  emitVerilog(
    new AcceleratorTop(JsonPathConfig(Seq("sensor", "temperature"))),
    Array("--target-dir", "generated")
  )
}
