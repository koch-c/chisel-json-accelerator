import chisel3._
import chisel3.util._
import java.nio.charset.StandardCharsets.UTF_8

/** Observe the assembler's frozen slots without owning the record handshake. */
class RecordFilter(config: JsonPathConfig) extends Module {
  val io = IO(new Bundle {
    val record = Input(Vec(config.recordBytes, UInt(8.W)))
    val keep = Output(Bool())
  })

  val predicates = config.resolvedFilters.map { case (id, filter) =>
    val shift = 2 * (id % 4)
    val valid = io.record(id / 4)(shift + 1, shift) === 1.U
    val base = config.statusBytes + config.offsets(id)
    val matches = filter.value match {
      case IntLit(value) =>
        val bytes = config.sizes(id)
        val field =
          Cat((0 until bytes).reverse.map(i => io.record(base + i))).asSInt
        val constant = value.S((8 * bytes).W)
        filter.op match {
          case CompareOp.Eq => field === constant
          case CompareOp.Lt => field < constant
          case CompareOp.Le => field <= constant
          case CompareOp.Gt => field > constant
          case CompareOp.Ge => field >= constant
        }
      case StrLit(text) =>
        val bytes = text.getBytes(UTF_8)
        val length = Cat(io.record(base + 1), io.record(base))
        bytes.zipWithIndex.foldLeft(length === bytes.length.U) {
          case (same, (byte, i)) =>
            same && io.record(base + 2 + i) === (byte & 0xff).U
        }
    }
    valid && matches
  }
  io.keep := predicates.foldLeft(true.B)(_ && _)
}
