import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class NumberSerializerSpec extends AnyFlatSpec with ChiselScalatestTester {

  /** The 12 bytes expected for a number, following the frame layout. */
  def frame(
      negative: Boolean,
      isFloat: Boolean,
      overflow: Boolean,
      value: BigInt,
      exp10: Int,
      notANumber: Boolean = false
  ): Seq[Int] = {
    val flags = (if (negative) 1 else 0) | (if (isFloat) 2
                                            else 0) | (if (overflow) 4 else 0) |
      (if (notANumber) 8 else 0)
    Seq(0, flags) ++
      (0 until 8).map(i => ((value >> (8 * i)) & 0xff).toInt) ++
      (0 until 2).map(i => (exp10 >> (8 * i)) & 0xff)
  }

  def submit(
      c: NumberSerializer,
      id: Int,
      negative: Boolean,
      integer: BigInt,
      mantissa: BigInt,
      exp10: Int,
      isFloat: Boolean,
      overflow: Boolean,
      notANumber: Boolean = false
  ): Unit = {
    c.io.in.ready.expect(true.B)
    c.io.in.bits.attributeId.poke(id.U)
    c.io.in.bits.negative.poke(negative.B)
    c.io.in.bits.integer.poke(integer.U)
    c.io.in.bits.mantissa.poke(mantissa.U)
    c.io.in.bits.exp10.poke(exp10.S)
    c.io.in.bits.isFloat.poke(isFloat.B)
    c.io.in.bits.overflow.poke(overflow.B)
    c.io.in.bits.notANumber.poke(notANumber.B)
    c.io.in.valid.poke(true.B)
    c.clock.step()
    c.io.in.valid.poke(false.B)
  }

  def collect(
      c: NumberSerializer,
      expected: Seq[Int],
      stall: Boolean = false
  ): Unit = {
    for ((byte, index) <- expected.zipWithIndex) {
      c.io.out.valid.expect(true.B)
      c.io.in.ready.expect(false.B)
      if (stall) {
        c.io.out.ready.poke(false.B)
        for (_ <- 0 until 3) {
          c.io.out.bits.expect(byte.U)
          c.clock.step()
          c.io.out.valid.expect(true.B)
        }
      }
      c.io.out.bits.expect(byte.U)
      c.io.last.expect((index == expected.size - 1).B)
      c.io.out.ready.poke(true.B)
      c.clock.step()
      c.io.out.ready.poke(false.B)
    }
    c.io.out.valid.expect(false.B)
    c.io.last.expect(false.B)
    c.io.in.ready.expect(true.B)
  }

  behavior of "NumberSerializer"

  it should "be idle after reset" in {
    test(new NumberSerializer) { c =>
      c.io.in.ready.expect(true.B)
      c.io.out.valid.expect(false.B)
      c.io.last.expect(false.B)
    }
  }

  it should "send an integer as a 12-byte frame" in {
    test(new NumberSerializer) { c =>
      submit(
        c,
        id = 3,
        negative = false,
        integer = 25,
        mantissa = 99,
        exp10 = 0,
        isFloat = false,
        overflow = false
      )
      collect(
        c,
        frame(
          negative = false,
          isFloat = false,
          overflow = false,
          value = 25,
          exp10 = 0
        )
      )
    }
  }

  it should "send the mantissa and a negative exponent for a float" in {
    test(new NumberSerializer) { c =>
      submit(
        c,
        id = 7,
        negative = true,
        integer = 12,
        mantissa = 1234,
        exp10 = -2,
        isFloat = true,
        overflow = false
      )
      collect(c, Seq(0, 3, 0xd2, 0x04, 0, 0, 0, 0, 0, 0, 0xfe, 0xff))
    }
  }

  it should "send every value byte least significant first" in {
    test(new NumberSerializer) { c =>
      val value = BigInt("0102030405060708", 16)
      submit(
        c,
        id = 255,
        negative = false,
        integer = value,
        mantissa = 0,
        exp10 = 0,
        isFloat = false,
        overflow = true
      )
      collect(c, Seq(0, 4, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0))
    }
  }

  it should "hold each byte stable under backpressure and accept a second number" in {
    test(new NumberSerializer) { c =>
      submit(
        c,
        id = 1,
        negative = false,
        integer = 0,
        mantissa = 5,
        exp10 = -3,
        isFloat = true,
        overflow = false
      )
      collect(
        c,
        frame(
          negative = false,
          isFloat = true,
          overflow = false,
          value = 5,
          exp10 = -3
        ),
        stall = true
      )
      submit(
        c,
        id = 2,
        negative = true,
        integer = 7,
        mantissa = 0,
        exp10 = 0,
        isFloat = false,
        overflow = false
      )
      collect(
        c,
        frame(
          negative = true,
          isFloat = false,
          overflow = false,
          value = 7,
          exp10 = 0
        )
      )
    }
  }

  it should "send notANumber as flag bit 3" in {
    test(new NumberSerializer) { c =>
      submit(
        c,
        id = 9,
        negative = false,
        integer = 0,
        mantissa = 0,
        exp10 = 0,
        isFloat = false,
        overflow = false,
        notANumber = true
      )
      collect(c, Seq(0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    }
  }

  /** The frame for an integer type: header, then the value in `bytes` bytes. */
  def intFrame(flags: Int, value: BigInt, bytes: Int): Seq[Int] =
    Seq(0, flags) ++ (0 until bytes).map(i => ((value >> (8 * i)) & 0xff).toInt)

  def submitInt(
      c: NumberSerializer,
      id: Int,
      negative: Boolean,
      magnitude: BigInt,
      isFloat: Boolean = false
  ): Unit =
    submit(
      c,
      id,
      negative,
      magnitude,
      mantissa = 0,
      exp10 = 0,
      isFloat = isFloat,
      overflow = false
    )

  it should "send an Int32 in two's complement" in {
    test(new NumberSerializer(ValueType.Int32)) { c =>
      submitInt(c, 1, negative = false, magnitude = 25)
      collect(c, Seq(0, 0, 25, 0, 0, 0))
      submitInt(c, 2, negative = true, magnitude = 7)
      collect(c, Seq(0, 1, 0xf9, 0xff, 0xff, 0xff))
      submitInt(c, 3, negative = true, magnitude = 0)
      collect(c, Seq(0, 1, 0, 0, 0, 0))
    }
  }

  it should "flag an Int32 outside its range" in {
    test(new NumberSerializer(ValueType.Int32)) { c =>
      val max = (BigInt(1) << 31) - 1
      submitInt(c, 0, negative = false, magnitude = max)
      collect(c, intFrame(0, max, 4))
      submitInt(c, 0, negative = false, magnitude = max + 1)
      collect(c, intFrame(4, max + 1, 4))
      submitInt(c, 0, negative = true, magnitude = max + 1)
      collect(c, Seq(0, 1, 0, 0, 0, 0x80))
      submitInt(c, 0, negative = true, magnitude = max + 2)
      collect(c, intFrame(5, -(max + 2), 4))
    }
  }

  it should "send an Int64 in two's complement and flag its range" in {
    test(new NumberSerializer(ValueType.Int64)) { c =>
      val max = (BigInt(1) << 63) - 1
      submitInt(c, 0, negative = true, magnitude = 1)
      collect(c, Seq(0, 1) ++ Seq.fill(8)(0xff))
      submitInt(c, 0, negative = false, magnitude = max)
      collect(c, intFrame(0, max, 8))
      submitInt(c, 0, negative = false, magnitude = max + 1)
      collect(c, intFrame(4, max + 1, 8))
      submitInt(c, 0, negative = true, magnitude = max + 1)
      collect(c, intFrame(1, -(max + 1), 8))
      submitInt(c, 0, negative = true, magnitude = max + 2)
      collect(c, intFrame(5, -(max + 2), 8))
    }
  }

  it should "keep isFloat set when an integer type receives a fraction" in {
    test(new NumberSerializer(ValueType.Int32)) { c =>
      submitInt(c, 0, negative = false, magnitude = 12, isFloat = true)
      collect(c, Seq(0, 2, 12, 0, 0, 0))
    }
  }
}
