import chisel3._
import chisel3.util._

/** Sends one ParsedNumber as a binary frame. All values are sent least
  * significant byte first.
  *
  * byte 0 attribute id byte 1 flags: bit 0 negative, bit 1 isFloat, bit 2
  * overflow, bit 3 notANumber
  *
  * The rest depends on the value type: Number 8 bytes integer (mantissa for a
  * float), 2 bytes exp10 Int32 4 bytes two's complement Int64 8 bytes two's
  * complement
  *
  * For Int32 and Int64, overflow is also set when the number is outside the
  * type's range, and a number with a fraction or exponent keeps isFloat set:
  * the value is then only its integer digits and should not be used.
  */
class NumberSerializer(valueType: ValueType = ValueType.Number) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new ParsedNumber))
    val out = Decoupled(UInt(8.W))
    val last = Output(Bool()) // out carries the final byte of the frame
  })

  private def bytesOf(x: UInt, count: Int): Seq[UInt] =
    (0 until count).map(i => x(8 * i + 7, 8 * i))

  val n = io.in.bits

  /** The number as a two's complement integer, and whether it is out of range.
    */
  private def integer(width: Int): (Seq[UInt], Bool) = {
    val limit = (BigInt(1) << (width - 1)).U(64.W) // 2^(width - 1)
    val outOfRange = Mux(n.negative, n.integer > limit, n.integer >= limit)
    val signed = Mux(n.negative, 0.U - n.integer, n.integer)
    (bytesOf(signed, width / 8), outOfRange)
  }

  val (payload, outOfRange) = valueType match {
    case ValueType.Int32 => integer(32)
    case ValueType.Int64 => integer(64)
    case _               =>
      val value = Mux(n.isFloat, n.mantissa, n.integer)
      (bytesOf(value, 8) ++ bytesOf(n.exp10.asUInt, 2), false.B)
  }
  val flags =
    Cat(0.U(4.W), n.notANumber, n.overflow || outOfRange, n.isFloat, n.negative)

  val frameBytes = 2 + payload.size
  val busy = RegInit(false.B)
  val index = RegInit(0.U(log2Ceil(frameBytes).W))
  val frame = Reg(Vec(frameBytes, UInt(8.W)))

  io.in.ready := !busy
  io.out.valid := busy
  io.out.bits := frame(index)
  io.last := busy && index === (frameBytes - 1).U

  // Take a number and store the whole frame.
  when(io.in.fire) {
    frame := VecInit(Seq(n.attributeId, flags) ++ payload)
    index := 0.U
    busy := true.B
  }

  // Send the frame one byte at a time.
  when(io.out.fire) {
    index := index + 1.U
    when(index === (frameBytes - 1).U) { busy := false.B }
  }
}
