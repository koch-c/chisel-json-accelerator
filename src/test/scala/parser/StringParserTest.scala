import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class StringParserSpec extends AnyFlatSpec with ChiselScalatestTester {
  val notAString = 1
  val badEscape = 2

  /** Drives the input and collects the frames accepted on the output.
    * stallOutput is the number of cycles a valid byte is held back before it is
    * accepted, to exercise backpressure. inputGap is the number of idle cycles
    * between two input bytes.
    */
  private class Driver(
      c: StringParser,
      stallOutput: Int = 0,
      inputGap: Int = 0
  ) {
    val frames = scala.collection.mutable.ArrayBuffer.empty[Seq[Int]]
    private val current = scala.collection.mutable.ArrayBuffer.empty[Int]
    private var stalled = 0

    c.io.in.valid.poke(false.B)
    c.io.in.bits.data.poke(0.U)
    c.io.in.bits.attributeId.poke(0.U)
    c.io.in.bits.last.poke(false.B)
    c.io.out.ready.poke(false.B)

    /** Runs one cycle and returns whether the input byte was taken in it. */
    def tick(): Boolean = {
      val valid = c.io.out.valid.peek().litToBoolean
      val ready = stalled >= stallOutput
      c.io.out.ready.poke(ready.B)
      if (valid && ready) {
        current += c.io.out.bits.peek().litValue.toInt
        if (c.io.last.peek().litToBoolean) {
          frames += current.toSeq
          current.clear()
        }
        stalled = 0
      } else if (valid) {
        stalled += 1
      }
      val taken =
        c.io.in.valid.peek().litToBoolean && c.io.in.ready.peek().litToBoolean
      c.clock.step()
      taken
    }

    /** Sends the attribute bytes one at a time, with `last` on the final byte.
      */
    def feed(text: String, id: Int): Unit = {
      val bytes = text.getBytes("UTF-8")
      for ((byte, index) <- bytes.zipWithIndex) {
        c.io.in.valid.poke(true.B)
        c.io.in.bits.data.poke((byte & 0xff).U)
        c.io.in.bits.attributeId.poke(id.U)
        c.io.in.bits.last.poke((index == bytes.length - 1).B)
        var waited = 0
        while (!tick() && waited < 50) waited += 1
        assert(waited < 50, s"Input stalled at byte $index of '$text'")
        c.io.in.valid.poke(false.B)
        c.io.in.bits.last.poke(false.B)
        for (_ <- 0 until inputGap) tick()
      }
    }

    /** Sends one attribute and returns the single frame it must produce. */
    def parse(text: String, id: Int = 0): Seq[Int] = {
      val before = frames.size
      feed(text, id)
      var waited = 0
      while (frames.size == before && waited < 50) {
        tick()
        waited += 1
      }
      for (_ <- 0 until 4) tick()
      assert(
        frames.size == before + 1,
        s"Expected exactly one frame for '$text', got ${frames.size - before}"
      )
      assert(current.isEmpty, s"Bytes after the frame for '$text': $current")
      frames.last
    }
  }

  /** The frame for a decoded string. */
  private def frame(id: Int, text: String, flags: Int = 0): Seq[Int] =
    Seq(id) ++ text.getBytes("UTF-8").map(_ & 0xff) ++ Seq(0, flags)

  private def check(
      d: Driver,
      attribute: String,
      text: String,
      flags: Int = 0,
      id: Int = 0
  ): Unit = {
    val got = d.parse(attribute, id)
    assert(got == frame(id, text, flags), s"for '$attribute'")
  }

  behavior of "StringParser"

  it should "be idle after reset" in {
    test(new StringParser) { c =>
      c.io.in.valid.poke(false.B)
      c.io.out.valid.expect(false.B)
      c.io.last.expect(false.B)
    }
  }

  it should "send a plain string without its quotes" in {
    test(new StringParser) { c =>
      val d = new Driver(c)
      check(d, ":\"hello\"", "hello", id = 3)
    }
  }

  it should "send an empty string as an empty frame" in {
    test(new StringParser) { c =>
      val d = new Driver(c)
      check(d, ":\"\"", "", id = 9)
    }
  }

  it should "skip whitespace around the string but keep it inside" in {
    test(new StringParser) { c =>
      val d = new Driver(c)
      check(d, " : \t\"a b \"  ", "a b ")
      check(d, ":\"x\"\r\n", "x")
    }
  }

  it should "decode the simple escapes" in {
    test(new StringParser) { c =>
      val d = new Driver(c)
      check(d, ": \"a\\\"b\"", "a\"b")
      check(d, ": \"a\\\\b\"", "a\\b")
      check(d, ": \"a\\/b\"", "a/b")
      check(d, ": \"\\b\\f\\n\\r\\t\"", "\b\f\n\r\t")
    }
  }

  it should "tell an escaped quote from the closing quote" in {
    test(new StringParser) { c =>
      val d = new Driver(c)
      check(
        d,
        ":\"a\\\\\"",
        "a\\"
      ) // "a\\" : escaped backslash, then the closing quote
      check(
        d,
        ":\"a\\\"\"",
        "a\""
      ) // "a\"" : escaped quote, then the closing quote
      check(d, ":\"\\\\\\\"\"", "\\\"") // "\\\"" : both
    }
  }

  it should "keep structural characters inside the string" in {
    test(new StringParser) { c =>
      val d = new Driver(c)
      check(d, ": \"{a:[1,2]}\"", "{a:[1,2]}")
    }
  }

  it should "pass UTF-8 bytes through unchanged" in {
    test(new StringParser) { c =>
      val d = new Driver(c)
      check(d, ": \"blåbærgrød €\"", "blåbærgrød €")
    }
  }

  it should "flag unicode and unknown escapes" in {
    test(new StringParser) { c =>
      val d = new Driver(c)
      check(d, ": \"\\u0041\"", "u0041", flags = badEscape)
      check(d, ": \"a\\qb\"", "aqb", flags = badEscape)
      check(d, ": \"ok\"", "ok") // the flag does not stick
    }
  }

  it should "flag values that are not a string" in {
    test(new StringParser) { c =>
      val d = new Driver(c)
      check(d, ": 25", "", flags = notAString)
      check(d, ":null", "", flags = notAString)
      check(d, ":", "", flags = notAString)
      check(d, ": \"open", "open", flags = notAString)
      check(d, ": \"open\\", "open", flags = notAString)
      check(d, ": \"a\" x", "a", flags = notAString)
      check(d, ": x\"a\"", "a", flags = notAString)
      check(d, ": \"ok\"", "ok") // the flag does not stick
    }
  }

  it should "drop raw control characters and flag them" in {
    test(new StringParser) { c =>
      val d = new Driver(c)
      check(d, ": \"a\tb\"", "ab", flags = notAString)
      check(d, ": \"a\u0000b\"", "ab", flags = notAString)
      check(d, ": \"a\\\u0000b\"", "ab", flags = notAString | badEscape)
    }
  }

  it should "send one frame per attribute with the right id" in {
    test(new StringParser) { c =>
      val d = new Driver(c)
      check(d, ":\"one\"", "one", id = 1)
      check(d, ":\"two\"", "two", id = 2)
      check(d, ": 3", "", flags = notAString, id = 3)
      assert(d.frames.size == 3)
    }
  }

  it should "hold its bytes under output backpressure" in {
    test(new StringParser) { c =>
      val d = new Driver(c, stallOutput = 3)
      check(d, ": \"a\\nb\" ", "a\nb", id = 7)
      check(d, ":\"\"", "", id = 8)
    }
  }

  it should "wait for input bytes that arrive with gaps" in {
    test(new StringParser) { c =>
      val d = new Driver(c, stallOutput = 1, inputGap = 2)
      check(d, ": \"a\\tb\" ", "a\tb", id = 4)
    }
  }
}
