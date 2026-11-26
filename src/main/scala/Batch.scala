import chisel3._
import chisel3.util._


class Batch(width: Int = 8) extends Bundle {
  val bytes = Vec(width, UInt(8.W))
  val count = UInt(log2Ceil(width + 1).W)
}