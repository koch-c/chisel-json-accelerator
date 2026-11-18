import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class NumberParserSpec extends AnyFlatSpec with ChiselScalatestTester {
  case class Result(
    id: Int,
    negative: Boolean,
    integer: BigInt,
    mantissa: BigInt,
    exp10: Int,
    isFloat: Boolean
  )

  /** Drives the input and collects every ParsedNumber accepted on the output.
    * stallOutput is the number of cycles a valid result is held back before it
    * is accepted, to exercise backpressure.
    */
  private class Driver(c: NumberParser, stallOutput: Int = 0) {
    val results = scala.collection.mutable.ArrayBuffer.empty[Result]
    private var stalled = 0

    c.io.in.valid.poke(false.B)
    c.io.in.bits.data.poke(0.U)
    c.io.in.bits.attributeId.poke(0.U)
    c.io.in.bits.last.poke(false.B)
    c.io.out.ready.poke(false.B)

    def tick(): Unit = {
      val valid = c.io.out.valid.peek().litToBoolean
      val ready = valid && stalled >= stallOutput
      c.io.out.ready.poke(ready.B)
      if (valid && ready) {
        results += Result(
          c.io.out.bits.attributeId.peek().litValue.toInt,
          c.io.out.bits.negative.peek().litToBoolean,
          c.io.out.bits.integer.peek().litValue,
          c.io.out.bits.mantissa.peek().litValue,
          c.io.out.bits.exp10.peek().litValue.toInt,
          c.io.out.bits.isFloat.peek().litToBoolean
        )
        stalled = 0
      } else if (valid) {
        stalled += 1
      }
      c.clock.step()
    }

    /** Sends the attribute bytes one at a time, with `last` on the final byte. */
    def feed(text: String, id: Int): Unit = {
      val bytes = text.getBytes("UTF-8")
      for ((byte, index) <- bytes.zipWithIndex) {
        c.io.in.valid.poke(true.B)
        c.io.in.bits.data.poke((byte & 0xff).U)
        c.io.in.bits.attributeId.poke(id.U)
        c.io.in.bits.last.poke((index == bytes.length - 1).B)
        var waited = 0
        while (!c.io.in.ready.peek().litToBoolean && waited < 50) {
          tick()
          waited += 1
        }
        assert(waited < 50, s"Input stalled at byte $index of '$text'")
        tick()
      }
      c.io.in.valid.poke(false.B)
      c.io.in.bits.last.poke(false.B)
    }

    /** Sends one attribute and returns the single result it must produce. */
    def parse(text: String, id: Int = 0): Result = {
      val before = results.size
      feed(text, id)
      var waited = 0
      while (results.size == before && waited < 50) {
        tick()
        waited += 1
      }
      for (_ <- 0 until 4) tick()
      assert(results.size == before + 1,
        s"Expected exactly one result for '$text', got ${results.size - before}")
      results.last
    }
  }

  behavior of "NumberParser"

  it should "be idle after reset" in {
    test(new NumberParser) { c =>
      c.io.in.ready.expect(true.B)
      c.io.out.valid.expect(false.B)
    }
  }

  // Change `ignore` to `it` once TODO 1 to 3 and 7 are in place.
  ignore should "parse a plain integer" in {
    test(new NumberParser) { c =>
      val d = new Driver(c)
      val r = d.parse(": 25", id = 3)
      assert(r.id == 3)
      assert(!r.negative)
      assert(r.integer == 25)
      assert(!r.isFloat)
    }
  }

  // TODO: add a test for each step as it lands. Suggested cases:
  //  - a negative integer                         ":-7"
  //  - no whitespace and trailing whitespace      ":25" and ": 25  "
  //  - a fraction                                 ": 12.34"
  //  - an exponent with and without a sign        ":1e3", ":-1.25e+2  ", ":5E-3"
  //  - zero and a leading-zero fraction           ":0", ":0.05"
  //  - two attributes in a row, with different ids (is everything cleared?)
  //  - output backpressure                        new Driver(c, stallOutput = 3)
  //  - the largest value that fits in the integer width
}
