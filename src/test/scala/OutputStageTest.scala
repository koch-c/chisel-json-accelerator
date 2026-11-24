import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable

class OutputStageSpec extends AnyFlatSpec with ChiselScalatestTester {
  private case class Entry(id: Int, endOfRecord: Boolean)
  private case class Beat(data: Int, empty: Boolean, last: Boolean, ok: Boolean)
  private case class Scalar(value: BigInt, ok: Boolean)

  /** Plays the reorder FIFO and the three value FIFOs, and collects the frames. */
  private class Driver(c: OutputStage) {
    val reorder = mutable.Queue.empty[Entry]
    val strings = mutable.Queue.empty[Beat]
    val ints = mutable.Queue.empty[Scalar]
    val floats = mutable.Queue.empty[Scalar]
    val output = mutable.ArrayBuffer.empty[Int]
    var done = 0
    var dropped = 0
    var allowOutput = true
    var stall = false
    var holdReorder = false
    var holdValues = false
    private var cycles = 0
    private var heldByte: Option[Int] = None

    def attribute(id: Int): Unit = reorder += Entry(id, endOfRecord = false)
    def endOfRecord(): Unit = reorder += Entry(0, endOfRecord = true)
    def string(text: String, ok: Boolean = true): Unit = {
      val bytes = text.getBytes("UTF-8").map(_ & 0xff)
      if (bytes.isEmpty) strings += Beat(0, empty = true, last = true, ok)
      for ((byte, i) <- bytes.zipWithIndex) {
        strings += Beat(byte, empty = false, last = i == bytes.length - 1, ok)
      }
    }
    def int(value: BigInt, ok: Boolean = true): Unit = ints += Scalar(value, ok)
    def float(bits: BigInt, ok: Boolean = true): Unit = floats += Scalar(bits, ok)

    def tick(): Unit = {
      val entry = if (holdReorder) None else reorder.headOption
      c.io.reorder.valid.poke(entry.nonEmpty.B)
      entry.foreach { e =>
        c.io.reorder.bits.attributeId.poke(e.id.U)
        c.io.reorder.bits.endOfRecord.poke(e.endOfRecord.B)
      }
      val beat = if (holdValues) None else strings.headOption
      c.io.strings.valid.poke(beat.nonEmpty.B)
      beat.foreach { b =>
        c.io.strings.bits.data.poke(b.data.U)
        c.io.strings.bits.empty.poke(b.empty.B)
        c.io.strings.bits.last.poke(b.last.B)
        c.io.strings.bits.ok.poke(b.ok.B)
      }
      val integer = if (holdValues) None else ints.headOption
      c.io.ints.valid.poke(integer.nonEmpty.B)
      integer.foreach { v =>
        c.io.ints.bits.value.poke(v.value.S(64.W))
        c.io.ints.bits.ok.poke(v.ok.B)
      }
      val real = if (holdValues) None else floats.headOption
      c.io.floats.valid.poke(real.nonEmpty.B)
      real.foreach { v =>
        c.io.floats.bits.bits.poke(v.value.U(64.W))
        c.io.floats.bits.ok.poke(v.ok.B)
      }

      val ready = allowOutput && (!stall || cycles % 5 == 0)
      c.io.out.ready.poke(ready.B)
      val valid = c.io.out.valid.peek().litToBoolean
      heldByte.foreach { byte =>
        assert(valid, "Output valid dropped while stalled")
        c.io.out.bits.expect(byte.U)
      }
      if (valid) {
        val byte = c.io.out.bits.peek().litValue.toInt
        if (ready) output += byte
        heldByte = if (ready) None else Some(byte)
      } else heldByte = None

      if (entry.nonEmpty && c.io.reorder.ready.peek().litToBoolean) reorder.dequeue()
      if (beat.nonEmpty && c.io.strings.ready.peek().litToBoolean) strings.dequeue()
      if (integer.nonEmpty && c.io.ints.ready.peek().litToBoolean) ints.dequeue()
      if (real.nonEmpty && c.io.floats.ready.peek().litToBoolean) floats.dequeue()
      if (c.io.recordDone.peek().litToBoolean) done += 1
      if (c.io.recordDropped.peek().litToBoolean) dropped += 1
      c.clock.step()
      cycles += 1
    }

    def idle(cycles: Int): Unit = for (_ <- 0 until cycles) tick()

    def waitFor(condition: => Boolean): Unit = {
      var waited = 0
      while (!condition && waited < 5000) {
        tick()
        waited += 1
      }
      assert(condition, "Timed out waiting for the output stage")
    }

    /** Waits for the frames, then checks that exactly those bytes came out. */
    def expectFrames(frames: Seq[Int]*): Unit = {
      val expected = frames.flatten
      waitFor(output.length >= expected.length)
      idle(30)
      assert(output.toSeq == expected, s"Unexpected output: ${output.toSeq}")
      output.clear()
    }
  }

  private def run(layout: OutputLayout)(body: Driver => Unit): Unit = {
    test(new OutputStage(layout)) { c =>
      c.io.reorder.valid.poke(false.B)
      c.io.strings.valid.poke(false.B)
      c.io.ints.valid.poke(false.B)
      c.io.floats.valid.poke(false.B)
      c.io.out.ready.poke(true.B)
      body(new Driver(c))
    }
  }

  private def le(value: BigInt, bytes: Int): Seq[Int] =
    (0 until bytes).map(i => ((value >> (8 * i)) & 0xff).toInt)
  private def text(value: String, bytes: Int): Seq[Int] =
    value.getBytes("UTF-8").map(_ & 0xff).toSeq.padTo(bytes, 0)
  private def f32(value: Float): BigInt =
    BigInt(java.lang.Float.floatToIntBits(value)) & 0xffffffffL
  private def f64(value: Double): BigInt =
    BigInt(java.lang.Double.doubleToLongBits(value)) & ((BigInt(1) << 64) - 1)

  // $.(id::int(32) | user.name::string(8))
  private val idName = OutputLayout(Seq(IntAttr(32), StringAttr(8)))

  private def sendIdName(d: Driver, id: Option[BigInt], name: Option[String]): Unit = {
    id.foreach { value => d.attribute(0); d.int(value) }
    name.foreach { value => d.attribute(1); d.string(value) }
    d.endOfRecord()
  }
  private def idNameFrame(id: Option[BigInt], name: Option[String]): Seq[Int] = {
    val mask = (if (id.nonEmpty) 1 else 0) | (if (name.nonEmpty) 2 else 0)
    Seq(mask) ++ le(id.getOrElse(BigInt(0)), 4) ++ text(name.getOrElse(""), 8)
  }

  private def expectKept(d: Driver)(send: => Unit)(frame: Seq[Int]): Unit = {
    val (done, dropped) = (d.done, d.dropped)
    send
    d.waitFor(d.done == done + 1)
    d.expectFrames(frame)
    assert(d.dropped == dropped, "Record was dropped")
  }
  private def expectDropped(d: Driver)(send: => Unit): Unit = {
    val (done, dropped) = (d.done, d.dropped)
    send
    d.waitFor(d.done == done + 1)
    d.expectFrames()
    assert(d.dropped == dropped + 1, "Record was not dropped")
  }

  "OutputStage" should "serialize a record as presence mask plus fixed-offset fields" in {
    run(idName) { d =>
      sendIdName(d, Some(7), Some("john"))
      d.expectFrames(Seq(0x03, 0x07, 0, 0, 0, 0x6a, 0x6f, 0x68, 0x6e, 0, 0, 0, 0))
      assert(d.done == 1)
      assert(d.dropped == 0)
    }
  }

  it should "produce the same frame when attributes arrive in another order" in {
    run(idName) { d =>
      d.attribute(1)
      d.string("john")
      d.attribute(0)
      d.int(7)
      d.endOfRecord()
      d.expectFrames(idNameFrame(Some(7), Some("john")))
    }
  }

  it should "mark missing and unparsable attributes as absent with a zero field" in {
    run(idName) { d =>
      sendIdName(d, None, Some("john"))
      d.expectFrames(Seq(0x02, 0, 0, 0, 0) ++ text("john", 8))
      sendIdName(d, None, None)
      d.expectFrames(Seq.fill(13)(0))
      d.attribute(0)
      d.int(99, ok = false)
      d.endOfRecord()
      d.expectFrames(Seq.fill(13)(0))
      assert(d.done == 3)
    }
  }

  it should "treat an int that does not fit int(32) as absent" in {
    run(idName) { d =>
      sendIdName(d, Some(BigInt(Int.MaxValue)), None)
      d.expectFrames(idNameFrame(Some(BigInt(Int.MaxValue)), None))
      sendIdName(d, Some(BigInt(Int.MinValue)), None)
      d.expectFrames(idNameFrame(Some(BigInt(Int.MinValue)), None))
      sendIdName(d, Some(BigInt(1) << 31), None)
      d.expectFrames(Seq.fill(13)(0))
      sendIdName(d, Some(-(BigInt(1) << 31) - 1), None)
      d.expectFrames(Seq.fill(13)(0))
    }
  }

  it should "let the last duplicate win without leaving a stale tail" in {
    run(idName) { d =>
      d.attribute(1)
      d.string("johnny")
      d.attribute(0)
      d.int(1)
      d.attribute(1)
      d.string("al")
      d.attribute(0)
      d.int(2)
      d.endOfRecord()
      d.expectFrames(idNameFrame(Some(2), Some("al")))
    }
  }

  it should "treat a string that is not ok as absent with a zero field" in {
    run(idName) { d =>
      d.attribute(0)
      d.int(7)
      d.attribute(1)
      d.string("john", ok = false)
      d.endOfRecord()
      d.expectFrames(idNameFrame(Some(7), None))
      d.attribute(1)
      d.string("0123456789", ok = false) // Also clears the truncated flag.
      d.endOfRecord()
      d.expectFrames(idNameFrame(None, None))
    }
  }

  it should "truncate long strings and keep empty strings present" in {
    run(idName) { d =>
      sendIdName(d, None, Some("0123456789"))
      d.expectFrames(Seq(0x02, 0, 0, 0, 0) ++ text("01234567", 8))
      sendIdName(d, None, Some(""))
      d.expectFrames(Seq(0x02) ++ Seq.fill(12)(0))
    }
  }

  it should "write 64-bit ints and floats little-endian" in {
    run(OutputLayout(Seq(IntAttr(64), FloatAttr(32), FloatAttr(64)))) { d =>
      d.attribute(0)
      d.int(-2)
      d.attribute(1)
      d.float(f32(1.5f))
      d.attribute(2)
      d.float(f64(-2.5))
      d.endOfRecord()
      d.expectFrames(
        Seq(0x07) ++ Seq(0xfe) ++ Seq.fill(7)(0xff) ++
          Seq(0x00, 0x00, 0xc0, 0x3f) ++ Seq(0, 0, 0, 0, 0, 0, 0x04, 0xc0)
      )
      d.attribute(1)
      d.float(f32(1.5f), ok = false)
      d.endOfRecord()
      d.expectFrames(Seq.fill(21)(0))
    }
  }

  it should "use the second presence byte for attributes 8 and up" in {
    run(OutputLayout(Seq.fill(9)(IntAttr(32)))) { d =>
      d.attribute(8)
      d.int(5)
      d.attribute(0)
      d.int(6)
      d.endOfRecord()
      d.expectFrames(Seq(0x01, 0x01) ++ le(6, 4) ++ Seq.fill(28)(0) ++ le(5, 4))
    }
  }

  it should "accept a value before or after its reorder entry" in {
    run(idName) { d =>
      d.holdReorder = true
      sendIdName(d, Some(7), Some("john"))
      d.idle(10)
      assert(d.done == 0)
      assert(d.ints.size == 1, "Value was taken before its reorder entry")
      d.holdReorder = false
      d.expectFrames(idNameFrame(Some(7), Some("john")))

      d.holdValues = true
      sendIdName(d, Some(8), Some("jane"))
      d.idle(10)
      assert(d.done == 1)
      d.holdValues = false
      d.expectFrames(idNameFrame(Some(8), Some("jane")))
    }
  }

  it should "commit records while the consumer is stalled and keep frames intact" in {
    run(idName) { d =>
      d.allowOutput = false
      for (i <- 1 to 4) sendIdName(d, Some(i), Some(s"rec$i"))
      d.idle(120)
      // Frame 1 fills the out FIFO, record 2 sits in the out buffer, and
      // record 3 waits in the attribute registers for the buffer to free up.
      assert(d.done == 2)
      assert(d.output.isEmpty)
      assert(d.reorder.size == 3, "Record 4 was accepted before record 3 committed")

      d.allowOutput = true
      d.stall = true
      d.waitFor(d.done == 4)
      d.expectFrames((1 to 4).map(i => idNameFrame(Some(i), Some(s"rec$i"))): _*)
    }
  }

  it should "not leak fields or presence bits into the next record" in {
    run(idName) { d =>
      sendIdName(d, Some(-1), Some("abcdefgh"))
      sendIdName(d, None, None)
      d.expectFrames(idNameFrame(Some(-1), Some("abcdefgh")), Seq.fill(13)(0))
    }
  }

  it should "drop records whose int does not satisfy every comparison" in {
    val layout = idName.copy(filters = Seq(
      Comparison(0, CompareOp.Ge, IntLit(-5)),
      Comparison(0, CompareOp.Lt, IntLit(20))
    ))
    run(layout) { d =>
      for (id <- Seq(-5, -4, 0, 19)) {
        expectKept(d)(sendIdName(d, Some(id), Some("x")))(idNameFrame(Some(id), Some("x")))
      }
      for (id <- Seq(-6, 20, 21, Int.MinValue, Int.MaxValue)) {
        expectDropped(d)(sendIdName(d, Some(id), Some("x")))
      }
      expectDropped(d)(sendIdName(d, None, Some("x"))) // Absent attribute.
      // Dropping a record must not disturb the next one.
      expectKept(d)(sendIdName(d, Some(3), None))(idNameFrame(Some(3), None))
    }
  }

  it should "support every int comparison operator" in {
    def outcome(op: CompareOp, value: Int): Boolean = {
      var kept = false
      run(idName.copy(filters = Seq(Comparison(0, op, IntLit(42))))) { d =>
        sendIdName(d, Some(value), None)
        d.waitFor(d.done == 1)
        d.idle(30)
        kept = d.output.nonEmpty
        assert(kept != (d.dropped == 1))
      }
      kept
    }
    assert(Seq(41, 42, 43).map(outcome(CompareOp.Eq, _)) == Seq(false, true, false))
    assert(Seq(41, 42, 43).map(outcome(CompareOp.Lt, _)) == Seq(true, false, false))
    assert(Seq(41, 42, 43).map(outcome(CompareOp.Gt, _)) == Seq(false, false, true))
    assert(Seq(41, 42, 43).map(outcome(CompareOp.Le, _)) == Seq(true, true, false))
    assert(Seq(41, 42, 43).map(outcome(CompareOp.Ge, _)) == Seq(false, true, true))
  }

  it should "compare strings exactly, including truncated ones" in {
    val layout = OutputLayout(
      Seq(StringAttr(4), StringAttr(8)),
      Seq(Comparison(0, CompareOp.Eq, StrLit("john")), Comparison(1, CompareOp.Eq, StrLit("al")))
    )
    def send(d: Driver, first: String, second: String): Unit = {
      d.attribute(0)
      d.string(first)
      d.attribute(1)
      d.string(second)
      d.endOfRecord()
    }
    run(layout) { d =>
      expectKept(d)(send(d, "john", "al"))(Seq(0x03) ++ text("john", 4) ++ text("al", 8))
      expectDropped(d)(send(d, "johnny", "al")) // Only the first four bytes match.
      expectDropped(d)(send(d, "joh", "al"))
      expectDropped(d)(send(d, "john", "ale"))
      expectDropped(d)(send(d, "john", ""))
      expectKept(d)(send(d, "john", "al"))(Seq(0x03) ++ text("john", 4) ++ text("al", 8))
    }
  }

  it should "compare floats by value, not by bit pattern" in {
    val range = OutputLayout(
      Seq(FloatAttr(32)),
      Seq(Comparison(0, CompareOp.Ge, FloatLit(-0.65)), Comparison(0, CompareOp.Le, IntLit(38)))
    )
    def send(d: Driver, bits: BigInt): Unit = {
      d.attribute(0)
      d.float(bits)
      d.endOfRecord()
    }
    run(range) { d =>
      for (value <- Seq(-0.65f, -0.5f, -0.0f, 0.0f, 1.0e-40f, 37.99f, 38.0f)) {
        expectKept(d)(send(d, f32(value)))(Seq(0x01) ++ le(f32(value), 4))
      }
      for (value <- Seq(-0.66f, -1.0e30f, 38.01f, Float.PositiveInfinity,
          Float.NegativeInfinity, Float.NaN)) {
        expectDropped(d)(send(d, f32(value)))
      }
    }

    val zero = OutputLayout(
      Seq(FloatAttr(64)),
      Seq(Comparison(0, CompareOp.Eq, FloatLit(0.0)))
    )
    run(zero) { d =>
      expectKept(d)(send(d, f64(0.0)))(Seq(0x01) ++ le(f64(0.0), 8))
      expectKept(d)(send(d, f64(-0.0)))(Seq(0x01) ++ le(f64(-0.0), 8))
      expectDropped(d)(send(d, f64(Double.MinPositiveValue)))
      expectDropped(d)(send(d, f64(-2.5)))
      expectDropped(d)(send(d, f64(Double.NaN)))
    }
  }
}
