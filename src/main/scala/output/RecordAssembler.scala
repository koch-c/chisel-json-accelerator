import chisel3._
import chisel3.util._

/** Retain the head descriptor until its result has been fully consumed. The
  * terminal string beat carries flags, so an overlong string must still drain.
  */
class RecordAssembler(c: JsonPathConfig) extends Module {
  val io = IO(new Bundle {
    val reorder = Flipped(Decoupled(new DispatchEntry(c)))
    val numbers = Flipped(Vec(c.numbers, Decoupled(new ParsedNumber)))
    val strings = Flipped(Vec(c.strings, Decoupled(new TextBeat)))
    val record = Decoupled(Vec(c.recordBytes, UInt(8.W)))
  })
  val complete = RegInit(false.B)
  val status = RegInit(VecInit(Seq.fill(c.fields.size)(0.U(2.W))))
  val slots = Reg(Vec(c.sizes.sum, UInt(8.W)))
  val head = io.reorder.bits
  val active = io.reorder.valid && !complete
  io.reorder.ready := false.B
  when(active && head.endOfRecord) {
    complete := true.B
    io.reorder.ready := true.B
  }
  val count = RegInit(0.U(17.W)) // saturates above every supported capacity
  for ((n, i) <- io.numbers.zipWithIndex) {
    n.ready := active && !head.endOfRecord && head.channel === i.U
    when(n.fire) { io.reorder.ready := true.B }
    for ((f, j) <- c.fields.zipWithIndex if f.valueType != ValueType.String) {
      val width = c.sizes(j) * 8
      val limit = (BigInt(1) << (width - 1)).U
      val v = n.bits
      val range =
        v.overflow || Mux(v.negative, v.integer > limit, v.integer >= limit)
      val signed = Mux(v.negative, 0.U - v.integer, v.integer)
      when(n.fire && head.attributeId === j.U) {
        status(j) := Mux(v.notANumber || v.isFloat, 2.U, Mux(range, 3.U, 1.U))
        for (k <- 0 until c.sizes(j))
          slots(c.offsets(j) + k) := signed(8 * k + 7, 8 * k)
      }
    }
  }
  for ((s, i) <- io.strings.zipWithIndex) {
    s.ready := active && !head.endOfRecord && head.channel === (c.numbers + i).U
    when(s.fire) {
      when(s.bits.last) { io.reorder.ready := true.B; count := 0.U }
        .elsewhen(count =/= 65536.U) { count := count + 1.U }
    }
    for ((f, j) <- c.fields.zipWithIndex if f.valueType == ValueType.String) {
      val base = c.offsets(j)
      when(s.fire && head.attributeId === j.U) {
        when(s.bits.last) {
          status(j) := Mux(
            s.bits.data.orR,
            2.U,
            Mux(count > f.stringBytes.U, 3.U, 1.U)
          )
          slots(base) := count(7, 0); slots(base + 1) := count(15, 8)
        }
        for (k <- 0 until f.stringBytes) {
          when(!s.bits.last && count === k.U) {
            slots(base + 2 + k) := s.bits.data
          }
          when(s.bits.last && count <= k.U) { slots(base + 2 + k) := 0.U }
        }
      }
    }
  }
  io.record.valid := complete
  for (k <- 0 until c.statusBytes)
    io.record.bits(k) := Cat(
      status.slice(4 * k, (4 * k + 4) min c.fields.size).reverse
    )
  for (j <- c.fields.indices; k <- 0 until c.sizes(j)) {
    val pos = c.offsets(j) + k
    io.record
      .bits(c.statusBytes + pos) := Mux(status(j) === 1.U, slots(pos), 0.U)
  }
  when(io.record.fire) { complete := false.B; status.foreach(_ := 0.U) }
}

/** Borrow the assembled record until the consumer accepts its final byte. */
class RecordSerializer(size: Int) extends Module {
  val io = IO(new Bundle {
    val record = Flipped(Decoupled(Vec(size, UInt(8.W))))
    val out = Decoupled(UInt(8.W))
  })
  val index = RegInit(0.U(log2Ceil(size max 2).W))
  io.out.valid := io.record.valid
  io.out.bits := io.record.bits(index)
  io.record.ready := io.out.ready && index === (size - 1).U
  when(io.out.fire) { index := Mux(io.record.ready, 0.U, index + 1.U) }
}
