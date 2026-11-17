import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class InputFIFOSpec extends AnyFlatSpec with ChiselScalatestTester {
  private def expectedWords(input: Seq[Int]): Seq[Seq[Int]] = {
    val words = scala.collection.mutable.ArrayBuffer.empty[Seq[Int]]
    var partial = Vector.empty[Int]
    input.foreach { byte =>
      partial :+= byte
      if (partial.size == 8 || byte == 10) {
        words += partial.padTo(8, 32)
        partial = Vector.empty
      }
    }
    words.toSeq
  }

  private def exercise(input: Seq[Int], stall: Boolean = false): Unit = {
    test(new InputFIFO(depthWords = 2)) { c =>
      val expected = expectedWords(input)
      var sent = 0
      var received = 0
      var cycles = 0
      while ((sent < input.size || received < expected.size) && cycles < 1000) {
        c.io.in.valid.poke((sent < input.size).B)
        c.io.in.bits.poke(input.lift(sent).getOrElse(0).U)
        val ready = !stall || (cycles >= 30 && cycles % 7 < 3)
        c.io.out.ready.poke(ready.B)
        val accepted = sent < input.size && c.io.in.ready.peek().litToBoolean
        if (c.io.out.valid.peek().litToBoolean) {
          assert(received < expected.size, "Unexpected extra word")
          // Also check every stalled cycle: the head word must remain stable.
          expected(received).zipWithIndex.foreach { case (byte, lane) =>
            c.io.out.bits(lane).expect(byte.U)
          }
          if (ready) received += 1
        }
        c.clock.step()
        if (accepted) sent += 1
        cycles += 1
      }
      assert(sent == input.size && received == expected.size, "FIFO did not drain")
      c.io.in.valid.poke(false.B)
      c.io.out.ready.poke(true.B)
      c.clock.step(3)
      c.io.out.valid.expect(false.B)
    }
  }

  "InputFIFO" should "retain LF and space-pad at every word offset" in {
    val input = (0 until 8).flatMap(n => Seq.fill(n)('x'.toInt) :+ 10)
    exercise(input)
  }

  it should "preserve order under output stalls and input backpressure" in {
    exercise(("0123456789abcdef\n{}\n" * 3).map(_.toInt), stall = true)
  }

  it should "accept a continuous byte stream without word-boundary bubbles" in {
    test(new InputFIFO) { c =>
      c.io.out.ready.poke(true.B)
      for (i <- 0 until 32) {
        c.io.in.valid.poke(true.B)
        c.io.in.bits.poke((65 + i).U)
        c.io.in.ready.expect(true.B)
        c.clock.step()
      }
    }
  }

  for (terminator <- Seq('h'.toInt, 10)) {
    val description = if (terminator == 10) "LF" else "the eighth byte"
    it should s"accept seven partial bytes while full and backpressure $description" in {
      test(new InputFIFO(depthWords = 1)) { c =>
        val first = "ABCDEFGH".map(_.toInt)
        val partial = "abcdefg".map(_.toInt)
        c.io.out.ready.poke(false.B)
        c.io.in.valid.poke(true.B)

        // Fill the single completed-word slot, then the partial register.
        for (byte <- first ++ partial) {
          c.io.in.bits.poke(byte.U)
          c.io.in.ready.expect(true.B)
          c.clock.step()
        }

        // The completing byte cannot be accepted while the word queue is full.
        c.io.in.bits.poke(terminator.U)
        for (_ <- 0 until 4) {
          c.io.in.ready.expect(false.B)
          c.io.out.valid.expect(true.B)
          first.zipWithIndex.foreach { case (byte, lane) =>
            c.io.out.bits(lane).expect(byte.U)
          }
          c.clock.step()
        }

        // This Queue has no same-cycle dequeue lookahead: space is usable
        // on the cycle after the first word is consumed.
        c.io.out.ready.poke(true.B)
        c.io.in.ready.expect(false.B)
        c.clock.step()
        c.io.out.ready.poke(false.B)
        c.io.in.ready.expect(true.B)
        c.clock.step()
        c.io.in.valid.poke(false.B)

        c.io.out.valid.expect(true.B)
        (partial :+ terminator).zipWithIndex.foreach { case (byte, lane) =>
          c.io.out.bits(lane).expect(byte.U)
        }
        c.io.out.ready.poke(true.B)
        c.clock.step()
        c.io.out.valid.expect(false.B)
      }
    }
  }

  it should "flush consecutive empty lines without merging records" in {
    exercise("\n\n{}\n\n".map(_.toInt))
  }

  it should "preserve zero and high-bit bytes without treating them as delimiters" in {
    exercise(Seq(0, 255, 128, 13, 34, 92, 32, 1, 254, 10))
  }

  it should "flush a partial word immediately when LF is accepted" in {
    test(new InputFIFO) { c =>
      c.io.out.ready.poke(false.B)
      c.io.in.valid.poke(true.B)
      for (byte <- "abc".map(_.toInt)) {
        c.io.in.bits.poke(byte.U)
        c.io.in.ready.expect(true.B)
        c.clock.step()
      }
      c.io.in.valid.poke(false.B)
      c.clock.step(3)
      c.io.out.valid.expect(false.B)
      c.io.in.valid.poke(true.B)
      c.io.in.bits.poke(10.U)
      c.io.in.ready.expect(true.B)
      c.clock.step()
      c.io.in.valid.poke(false.B)
      c.io.out.valid.expect(true.B)
      Seq(97, 98, 99, 10, 32, 32, 32, 32).zipWithIndex.foreach {
        case (byte, lane) => c.io.out.bits(lane).expect(byte.U)
      }
    }
  }

  it should "clear both queued words and partial bytes on reset" in {
    test(new InputFIFO(depthWords = 1)) { c =>
      c.io.out.ready.poke(false.B)
      c.io.in.valid.poke(true.B)
      for (byte <- "ABCDEFGHxyz".map(_.toInt)) {
        c.io.in.bits.poke(byte.U)
        c.io.in.ready.expect(true.B)
        c.clock.step()
      }
      c.io.out.valid.expect(true.B)
      c.io.in.valid.poke(false.B)
      c.reset.poke(true.B)
      c.clock.step()
      c.reset.poke(false.B)
      c.io.out.valid.expect(false.B)
      c.io.in.valid.poke(true.B)
      c.io.in.bits.poke(10.U)
      c.clock.step()
      c.io.in.valid.poke(false.B)
      c.io.out.valid.expect(true.B)
      c.io.out.bits(0).expect(10.U)
      for (lane <- 1 until 8) c.io.out.bits(lane).expect(32.U)
    }
  }

  it should "hold an incomplete word until LF and discard it on reset" in {
    test(new InputFIFO) { c =>
      c.io.out.ready.poke(true.B)
      c.io.in.valid.poke(true.B)
      c.io.in.bits.poke('x'.toInt.U)
      c.clock.step()
      c.io.in.valid.poke(false.B)
      c.clock.step(5)
      c.io.out.valid.expect(false.B)
      c.reset.poke(true.B)
      c.clock.step()
      c.reset.poke(false.B)
      c.io.in.valid.poke(true.B)
      c.io.in.bits.poke(10.U)
      c.clock.step()
      c.io.in.valid.poke(false.B)
      c.io.out.valid.expect(true.B)
      c.io.out.bits(0).expect(10.U)
      for (i <- 1 until 8) c.io.out.bits(i).expect(32.U)
    }
  }
}
