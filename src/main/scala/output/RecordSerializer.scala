import chisel3._
import chisel3.util._

/** Borrow the assembled record until the consumer accepts its final byte. */
class RecordSerializer(size: Int) extends Module {
  val io = IO(new Bundle {
    val record = Flipped(Decoupled(Vec(size, UInt(8.W))))
    val keep = Input(Bool()) // stable while record.valid is asserted
    val out = Decoupled(UInt(8.W))
  })
  val header = RegInit(true.B)
  val index = RegInit(0.U(log2Ceil(size max 2).W))
  val last = Mux(header, !io.keep, index === (size - 1).U)
  io.out.valid := io.record.valid
  io.out.bits := Mux(header, io.keep.asUInt, io.record.bits(index))
  io.record.ready := io.out.ready && last
  when(io.out.fire) {
    when(last) {
      header := true.B
      index := 0.U
    }.elsewhen(header) {
      header := false.B
    }.otherwise {
      index := index + 1.U
    }
  }
}
