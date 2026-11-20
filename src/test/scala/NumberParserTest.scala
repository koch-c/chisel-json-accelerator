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
    isFloat: Boolean,
    overflow: Boolean,
    notANumber: Boolean
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
          c.io.out.bits.isFloat.peek().litToBoolean,
          c.io.out.bits.overflow.peek().litToBoolean,
          c.io.out.bits.notANumber.peek().litToBoolean
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

  it should "parse a plain integer" in {
    test(new NumberParser) { c =>
      val d = new Driver(c)
      val r = d.parse(": 25", id = 3)
      assert(r.id == 3)
      assert(!r.negative)
      assert(r.integer == 25)
      assert(!r.isFloat)
    }
  }

  /** Parses one attribute and checks the result. The mantissa and exponent
    * are only checked for floats, since they are not meaningful for integers.
    */
  private def check(d: Driver, text: String, negative: Boolean, integer: BigInt,
                    isFloat: Boolean = false, mantissa: BigInt = 0, exp10: Int = 0,
                    id: Int = 0): Unit = {
    val r = d.parse(text, id)
    assert(r.id == id, s"attribute id for '$text'")
    assert(!r.overflow, s"overflow for '$text'")
    assert(!r.notANumber, s"notANumber for '$text'")
    assert(r.negative == negative, s"sign for '$text'")
    assert(r.integer == integer, s"integer part for '$text'")
    assert(r.isFloat == isFloat, s"isFloat for '$text'")
    if (isFloat) {
      assert(r.mantissa == mantissa, s"mantissa for '$text'")
      assert(r.exp10 == exp10, s"exp10 for '$text'")
    }
  }

  it should "parse a negative integer" in {
    test(new NumberParser) { c =>
      val d = new Driver(c)
      check(d, ":-7", negative = true, integer = 7)
    }
  }

  it should "ignore whitespace around the number" in {
    test(new NumberParser) { c =>
      val d = new Driver(c)
      check(d, ":25", negative = false, integer = 25)
      check(d, ": 25  ", negative = false, integer = 25)
      check(d, ":\t\n 42\r\n", negative = false, integer = 42)
    }
  }

  it should "parse a fraction" in {
    test(new NumberParser) { c =>
      val d = new Driver(c)
      check(d, ": 12.34", negative = false, integer = 12,
        isFloat = true, mantissa = 1234, exp10 = -2)
    }
  }

  it should "parse zero and a fraction with leading zeros" in {
    test(new NumberParser) { c =>
      val d = new Driver(c)
      check(d, ":0", negative = false, integer = 0)
      check(d, ":0.05", negative = false, integer = 0,
        isFloat = true, mantissa = 5, exp10 = -2)
    }
  }

  it should "parse an exponent with and without a sign" in {
    test(new NumberParser) { c =>
      val d = new Driver(c)
      check(d, ":1e3", negative = false, integer = 1,
        isFloat = true, mantissa = 1, exp10 = 3)
      check(d, ":5E-3", negative = false, integer = 5,
        isFloat = true, mantissa = 5, exp10 = -3)
      check(d, ":-1.25e+2  ", negative = true, integer = 1,
        isFloat = true, mantissa = 125, exp10 = 0)
    }
  }

  it should "combine the fraction count with the exponent" in {
    test(new NumberParser) { c =>
      val d = new Driver(c)
      check(d, ":12.34e5", negative = false, integer = 12,
        isFloat = true, mantissa = 1234, exp10 = 3)
      check(d, ":12.34e-5", negative = false, integer = 12,
        isFloat = true, mantissa = 1234, exp10 = -7)
    }
  }

  it should "clear its state between attributes" in {
    test(new NumberParser) { c =>
      val d = new Driver(c)
      check(d, ":-1.5e-2", negative = true, integer = 1,
        isFloat = true, mantissa = 15, exp10 = -3, id = 1)
      check(d, ":7", negative = false, integer = 7, id = 2)
      check(d, ":3.5", negative = false, integer = 3,
        isFloat = true, mantissa = 35, exp10 = -1, id = 255)
    }
  }

  it should "hold the result stable under output backpressure" in {
    test(new NumberParser) { c =>
      val d = new Driver(c, stallOutput = 3)
      check(d, ": 12.34", negative = false, integer = 12,
        isFloat = true, mantissa = 1234, exp10 = -2, id = 4)
      check(d, ":-7", negative = true, integer = 7, id = 5)
    }
  }

  it should "parse the largest value that fits in 64 bits" in {
    test(new NumberParser) { c =>
      val d = new Driver(c)
      val max = (BigInt(1) << 64) - 1
      check(d, s":$max", negative = false, integer = max)
    }
  }

  it should "flag a number that does not fit" in {
    test(new NumberParser) { c =>
      val d = new Driver(c)
      assert(!d.parse(":18446744073709551615").overflow, "2^64 - 1 fits")
      assert(d.parse(":18446744073709551616").overflow, "2^64 does not fit")
      assert(d.parse(":0.123456789012345678901234").overflow, "mantissa too long")
      assert(!d.parse(":1e16383").overflow, "largest exponent")
      assert(d.parse(":1e16384").overflow, "exponent too large")
      assert(d.parse(":1e-99999999999999999999999").overflow, "exponent wraps the accumulator")
      assert(!d.parse(":7").overflow, "flag is cleared for the next number")
    }
  }

  it should "flag an attribute that is not a number" in {
    test(new NumberParser) { c =>
      val d = new Driver(c)
      for (text <- Seq(": \"hello\"", ": \"abc123\"", ":true", ":null", ":{", ":[1", ":  ", ":-", ":1e",
                       ":1.2.3", ":--5", ":1-2", ":+5", ":.5", ":0x1F")) {
        assert(d.parse(text).notANumber, s"'$text' is not a number")
      }
      assert(!d.parse(":7").notANumber, "flag is cleared for the next number")
    }
  }
}
