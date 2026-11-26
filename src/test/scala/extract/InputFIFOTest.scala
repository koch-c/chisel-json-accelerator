import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class InputFIFOSpec extends AnyFlatSpec with ChiselScalatestTester {
  private def pokeBatch(c: InputFIFO, bytes: Seq[Int]): Unit = {
    require(bytes.nonEmpty && bytes.length <= 8)
    c.io.in.bits.count.poke(bytes.length.U)
    for (lane <- 0 until 8) {
      c.io.in.bits
        .bytes(lane)
        .poke(
          (if (lane < bytes.length) bytes(lane) else 0).U
        )
    }
  }

  private def expectWord(c: InputFIFO, bytes: Seq[Int]): Unit = {
    val padded = bytes.padTo(8, 32)
    padded.zipWithIndex.foreach { case (byte, lane) =>
      c.io.out.bits(lane).expect(byte.U)
    }
  }

  "InputFIFO" should "enqueue full and partial batches as padded words" in {
    test(new InputFIFO(depthWords = 4)) { c =>
      c.io.out.ready.poke(false.B)
      c.io.in.valid.poke(true.B)

      val batches = Seq("ABCDEFGH".map(_.toInt), Seq(125, 10))
      batches.foreach { batch =>
        pokeBatch(c, batch)
        c.io.in.ready.expect(true.B)
        c.clock.step()
      }
      c.io.in.valid.poke(false.B)

      c.io.out.valid.expect(true.B)
      expectWord(c, batches.head)
      c.io.out.ready.poke(true.B)
      c.clock.step()
      c.io.out.valid.expect(true.B)
      expectWord(c, batches(1))
      c.clock.step()
      c.io.out.valid.expect(false.B)
    }
  }

  it should "hold the head word stable under output backpressure" in {
    test(new InputFIFO(depthWords = 2)) { c =>
      c.io.out.ready.poke(false.B)
      c.io.in.valid.poke(true.B)
      val first = "12345678".map(_.toInt)
      pokeBatch(c, first)
      c.io.in.ready.expect(true.B)
      c.clock.step()

      c.io.in.valid.poke(false.B)
      for (_ <- 0 until 4) {
        c.io.out.valid.expect(true.B)
        expectWord(c, first)
        c.clock.step()
      }

      c.io.out.ready.poke(true.B)
      c.clock.step()
      c.io.out.valid.expect(false.B)
    }
  }

  it should "backpressure when the word queue is full" in {
    test(new InputFIFO(depthWords = 1)) { c =>
      c.io.out.ready.poke(false.B)
      c.io.in.valid.poke(true.B)
      val first = "ABCDEFGH".map(_.toInt)
      pokeBatch(c, first)
      c.io.in.ready.expect(true.B)
      c.clock.step()

      pokeBatch(c, Seq(10))
      c.io.in.ready.expect(false.B)
      c.io.out.valid.expect(true.B)
      expectWord(c, first)

      c.io.out.ready.poke(true.B)
      c.clock.step()
      c.io.in.ready.expect(true.B)
      c.clock.step()
      c.io.in.valid.poke(false.B)
      c.io.out.valid.expect(true.B)
      expectWord(c, Seq(10))
    }
  }

  it should "reject zero-count and over-wide batches" in {
    test(new InputFIFO) { c =>
      c.io.in.valid.poke(true.B)
      c.io.in.bits.count.poke(0.U)
      c.io.in.ready.expect(false.B)

      c.io.in.bits.count.poke(9.U)
      c.io.in.ready.expect(false.B)
    }
  }
}
