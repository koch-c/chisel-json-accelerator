import chisel3._

/** UART wrapper around the byte-stream extractor. Send one JSON object line at
  * a time; each line including LF must fit in 4096 bytes. UART has no flow
  * control. Wait for the complete response before sending the next line.
  */
class AcceleratorTop(
    config: JsonPathConfig = JsonPathConfig(Seq(FieldSpec(Seq("id")))),
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

object FpgaProfile {
  val config: JsonPathConfig = JsonPathConfig(
    fields = Seq(
      FieldSpec(Seq("id"), ValueType.Int64),
      FieldSpec(Seq("user", "name"), ValueType.String, stringBytes = 32),
      FieldSpec(
        Seq("user", "profile", "city"),
        ValueType.String,
        stringBytes = 32
      ),
      FieldSpec(Seq("sensor", "status"), ValueType.String, stringBytes = 16)
    ),
    filters = Seq(
      FilterSpec(Seq("id"), CompareOp.Ge, IntLit(BigInt(1) << 32)),
      FilterSpec(Seq("sensor", "status"), CompareOp.Eq, StrLit("ready"))
    )
  )
}

object GenerateAcceleratorTop extends App {
  emitVerilog(
    new AcceleratorTop(FpgaProfile.config),
    Array("--target-dir", "generated")
  )
}
