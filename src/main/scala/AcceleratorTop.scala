import chisel3._
import chisel3.util._

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
  val batcher = Module(new ByteBatcher)
  val core = Module(new Accelerator(config))
  val transmitter = Module(new UartTx(clockHz, baudRate))

  receiver.io.rx := io.uartRx
  batcher.io.in <> receiver.io.out
  core.io.in <> batcher.io.out
  transmitter.io.in <> core.io.out
  io.uartTx := transmitter.io.tx
}

/** Packs UART bytes into full words, flushing a partial word at LF. */
class ByteBatcher extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(8.W)))
    val out = Decoupled(new Batch(8))
  })

  val bytes = Reg(Vec(8, UInt(8.W)))
  val count = RegInit(0.U(4.W))
  val endsLine = RegInit(false.B)

  io.out.valid := count === 8.U || endsLine
  io.out.bits.count := count
  io.out.bits.bytes := bytes
  io.in.ready := count < 8.U && !io.out.valid

  when(io.in.fire) {
    bytes(count(2, 0)) := io.in.bits
    count := count + 1.U
    when(io.in.bits === 10.U) {
      endsLine := true.B
    }
  }

  when(io.out.fire) {
    count := 0.U
    endsLine := false.B
  }
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
