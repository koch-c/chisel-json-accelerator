import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class WordUnpackerSpec extends AnyFlatSpec with ChiselScalatestTester {
  private case class Out(data: Char, last: Boolean, id: Int)

  /** Character i of the text goes in byte i. */
  private def word(text: String): BigInt =
    text.zipWithIndex.map { case (ch, i) => BigInt(ch.toInt) << (8 * i) }.sum

  private class Driver(c: WordUnpacker, stall: Boolean) {
    val out = scala.collection.mutable.ArrayBuffer[Out]()
    // The cycle in which each byte of out was accepted.
    val cycles = scala.collection.mutable.ArrayBuffer[Int]()
    private var cycle = 0
    c.io.in.valid.poke(false.B)

    /** One clock cycle. Returns true when the input word was accepted. */
    def tick(): Boolean = {
      val ready = !stall || cycle % 5 == 0
      c.io.out.ready.poke(ready.B)
      if (ready && c.io.out.valid.peek().litToBoolean) {
        out += Out(
          c.io.out.bits.data.peek().litValue.toChar,
          c.io.out.bits.last.peek().litToBoolean,
          c.io.out.bits.attributeId.peek().litValue.toInt
        )
        cycles += cycle
      }
      val accepted =
        c.io.in.valid.peek().litToBoolean && c.io.in.ready.peek().litToBoolean
      c.clock.step()
      cycle += 1
      accepted
    }

    def send(
        text: String,
        first: Int,
        lastByte: Int,
        last: Boolean,
        id: Int = 0
    ): Unit = {
      c.io.in.valid.poke(true.B)
      c.io.in.bits.data.poke(word(text).U(64.W))
      c.io.in.bits.firstByte.poke(first.U)
      c.io.in.bits.lastByte.poke(lastByte.U)
      c.io.in.bits.last.poke(last.B)
      c.io.in.bits.attributeId.poke(id.U)
      var guard = 0
      while (!tick()) { guard += 1; assert(guard < 200, "word never accepted") }
      c.io.in.valid.poke(false.B)
    }
  }

  private def run(stall: Boolean)(body: Driver => Unit): Unit =
    test(new WordUnpacker)(c => body(new Driver(c, stall)))

  behavior of "WordUnpacker"

  for (stall <- Seq(false, true)) {
    it should s"send bytes 2 to 5 of one word (stall = $stall)" in run(stall) {
      d =>
        d.send("..abcd..", first = 2, lastByte = 5, last = true)
        assert(d.out.map(_.data).mkString == "abcd")
        assert(d.out.map(_.last) == Seq(false, false, false, true))
    }

    it should s"send a single-byte attribute (stall = $stall)" in run(stall) {
      d =>
        d.send("...x....", first = 3, lastByte = 3, last = true, id = 7)
        assert(d.out.toSeq == Seq(Out('x', last = true, id = 7)))
    }

    it should s"continue an attribute in the next word (stall = $stall)" in run(
      stall
    ) { d =>
      d.send(".....abc", first = 5, lastByte = 7, last = false)
      d.send("def.....", first = 0, lastByte = 2, last = true)
      assert(d.out.map(_.data).mkString == "abcdef")
      assert(d.out.map(_.last) == Seq(false, false, false, false, false, true))
    }

    it should s"go from byte 7 of one word to byte 0 of the next (stall = $stall)" in run(
      stall
    ) { d =>
      d.send(".......a", first = 7, lastByte = 7, last = false)
      d.send("bc......", first = 0, lastByte = 1, last = true)
      assert(d.out.map(_.data).mkString == "abc")
      assert(d.out.map(_.last) == Seq(false, false, true))
    }

    it should s"keep two attributes apart (stall = $stall)" in run(stall) { d =>
      d.send("ab......", first = 0, lastByte = 1, last = true, id = 1)
      d.send("...cde..", first = 3, lastByte = 5, last = true, id = 2)
      assert(d.out.map(_.data).mkString == "abcde")
      assert(d.out.map(_.id) == Seq(1, 1, 2, 2, 2))
      assert(d.out.map(_.last) == Seq(false, true, false, false, true))
    }
  }

  it should "send one byte every cycle across a word boundary" in run(stall =
    false
  ) { d =>
    d.send(".....abc", first = 5, lastByte = 7, last = false)
    d.send("def.....", first = 0, lastByte = 2, last = true)
    assert(d.out.map(_.data).mkString == "abcdef")
    assert(d.cycles.toSeq == (d.cycles.head until d.cycles.head + 6))
  }
}
