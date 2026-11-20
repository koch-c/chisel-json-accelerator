import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class NumberSerializerSpec extends AnyFlatSpec with ChiselScalatestTester {
  /** The 12 bytes expected for a number, following the frame layout. */
  def frame(id: Int, negative: Boolean, isFloat: Boolean, overflow: Boolean,
            value: BigInt, exp10: Int): Seq[Int] = {
    val flags = (if (negative) 1 else 0) | (if (isFloat) 2 else 0) | (if (overflow) 4 else 0)
    Seq(id, flags) ++
      (0 until 8).map(i => ((value >> (8 * i)) & 0xff).toInt) ++
      (0 until 2).map(i => (exp10 >> (8 * i)) & 0xff)
  }

  def submit(c: NumberSerializer, id: Int, negative: Boolean, integer: BigInt,
             mantissa: BigInt, exp10: Int, isFloat: Boolean, overflow: Boolean): Unit = {
    c.io.in.ready.expect(true.B)
    c.io.in.bits.attributeId.poke(id.U)
    c.io.in.bits.negative.poke(negative.B)
    c.io.in.bits.integer.poke(integer.U)
    c.io.in.bits.mantissa.poke(mantissa.U)
    c.io.in.bits.exp10.poke(exp10.S)
    c.io.in.bits.isFloat.poke(isFloat.B)
    c.io.in.bits.overflow.poke(overflow.B)
    c.io.in.valid.poke(true.B)
    c.clock.step()
    c.io.in.valid.poke(false.B)
  }

  def collect(c: NumberSerializer, expected: Seq[Int], stall: Boolean = false): Unit = {
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
      submit(c, id = 3, negative = false, integer = 25, mantissa = 99, exp10 = 0,
        isFloat = false, overflow = false)
      collect(c, frame(3, negative = false, isFloat = false, overflow = false, value = 25, exp10 = 0))
    }
  }

  it should "send the mantissa and a negative exponent for a float" in {
    test(new NumberSerializer) { c =>
      submit(c, id = 7, negative = true, integer = 12, mantissa = 1234, exp10 = -2,
        isFloat = true, overflow = false)
      collect(c, Seq(7, 3, 0xd2, 0x04, 0, 0, 0, 0, 0, 0, 0xfe, 0xff))
    }
  }

  it should "send every value byte least significant first" in {
    test(new NumberSerializer) { c =>
      val value = BigInt("0102030405060708", 16)
      submit(c, id = 255, negative = false, integer = value, mantissa = 0, exp10 = 0,
        isFloat = false, overflow = true)
      collect(c, Seq(255, 4, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0))
    }
  }

  it should "hold each byte stable under backpressure and accept a second number" in {
    test(new NumberSerializer) { c =>
      submit(c, id = 1, negative = false, integer = 0, mantissa = 5, exp10 = -3,
        isFloat = true, overflow = false)
      collect(c, frame(1, negative = false, isFloat = true, overflow = false, value = 5, exp10 = -3),
        stall = true)
      submit(c, id = 2, negative = true, integer = 7, mantissa = 0, exp10 = 0,
        isFloat = false, overflow = false)
      collect(c, frame(2, negative = true, isFloat = false, overflow = false, value = 7, exp10 = 0))
    }
  }
}
