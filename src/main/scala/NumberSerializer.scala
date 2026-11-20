import chisel3._
import chisel3.util._

/** Sends one ParsedNumber as a fixed 12-byte frame.
  *
  *   byte 0       attribute id
  *   byte 1       flags: bit 0 negative, bit 1 isFloat, bit 2 overflow
  *   bytes 2-9    integer, or mantissa for a float, least significant byte first
  *   bytes 10-11  exp10 in two's complement, least significant byte first
  */
class NumberSerializer extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new ParsedNumber))
    val out = Decoupled(UInt(8.W))
    val last = Output(Bool()) // out carries the final byte of the frame
  })

  val frameBytes = 12
  val busy = RegInit(false.B)
  val index = RegInit(0.U(log2Ceil(frameBytes).W))
  val frame = Reg(Vec(frameBytes, UInt(8.W)))

  io.in.ready := !busy
  io.out.valid := busy
  io.out.bits := frame(index)
  io.last := busy && index === (frameBytes - 1).U

  // Cut the number into bytes.
  when(io.in.fire) {
    val n = io.in.bits
    val flags = Cat(0.U(5.W), n.overflow, n.isFloat, n.negative)
    val value = Mux(n.isFloat, n.mantissa, n.integer)
    val exponent = n.exp10.asUInt
    val bytes = Seq(n.attributeId, flags) ++
      (0 until 8).map(i => value(8 * i + 7, 8 * i)) ++
      (0 until 2).map(i => exponent(8 * i + 7, 8 * i))
    frame := VecInit(bytes)
    index := 0.U
    busy := true.B
  }

  // Send the frame one byte at a time.
  when(io.out.fire) {
    index := index + 1.U
    when(index === (frameBytes - 1).U) { busy := false.B }
  }
}