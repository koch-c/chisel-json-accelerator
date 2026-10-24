import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class UartTxSpec extends AnyFlatSpec with ChiselScalatestTester {
  val cyclesPerBit = 8

  def initialize(c: UartTx): Unit = {
    c.io.in.valid.poke(false.B)
    c.io.in.bits.poke(0.U)
    c.reset.poke(true.B)
    c.clock.step(2)
    c.reset.poke(false.B)
    c.clock.step(cyclesPerBit)
  }

  def receiveByte(c: UartTx, byte: Int): Unit = {
    c.clock.step(cyclesPerBit)
    for (bit <- 0 until 8) {
      for (_ <- 0 until cyclesPerBit) {
        c.clock.step()
        c.io.tx.expect(((byte >> bit) & 1).B)
      }
    }
    c.clock.step(cyclesPerBit)
  }

  // def receiveByte(c: UartTx, byte: Int, validStop: Boolean = true): Unit = {
  //   c.io.rx.poke(false.B)
  //   c.clock.step(cyclesPerBit)
  //   for (bit <- 0 until 8) {
  //     c.io.rx.poke((((byte >> bit) & 1) != 0).B)
  //     c.clock.step(cyclesPerBit)
  //   }
  //   c.io.rx.poke(validStop.B)
  //   c.clock.step(cyclesPerBit)
  // }

  "UartTx" should "transmit 1 when valid is never asserted" in {
    test(new UartTx(80, 10)) { c =>
      initialize(c)
      for (_ <- 0 until 100) {
        c.io.tx.expect(true.B)
        c.clock.step()
      }
    }
  }

  it should "not be ready while transmitting" in {
    test(new UartTx(80, 10)) { c =>
      initialize(c)
      c.io.in.bits.poke(0x96.U)
      c.io.in.valid.poke(true.B)
      // it is ready on the last cycle of the end bit
      for (_ <- 0 until cyclesPerBit * 10 - 1) { 
        c.clock.step()
        c.io.in.ready.expect(false.B)
      }
    }
  }

  it should "start with a low signal and end with a high signal" in {
    test(new UartTx(80, 10)) { c =>
      initialize(c)
      c.io.in.bits.poke(0x96.U)
      c.io.in.valid.poke(true.B)
      c.io.tx.expect(true.B)
      c.clock.step()
      c.io.in.valid.poke(false.B)
      for (_ <- 0 until cyclesPerBit - 1) {
        c.clock.step()
        c.io.tx.expect(false.B)
      }
      c.clock.step(cyclesPerBit * 8) // transmit data bits
      for (_ <- 0 until cyclesPerBit) {
        c.clock.step()
        c.io.tx.expect(true.B)
      }
      // should be back to idle now
      c.clock.step()
      c.io.tx.expect(true.B)
    }
  }

  it should "transmit bits in LSB format" in {
    test(new UartTx(80, 10)) { c =>
      initialize(c)
      c.io.in.bits.poke(0x96.U)
      c.io.in.valid.poke(true.B)
      // it now is done with transmitting the first byte
      receiveByte(c, 0x96)
    }
  }

  it should "transmit bits in back to back" in {
    test(new UartTx(80, 10)) { c =>
      initialize(c)
      c.io.in.bits.poke(0x96.U)
      c.io.in.valid.poke(true.B)
      // it now is done with transmitting the first byte
      receiveByte(c, 0x96)
      c.io.in.bits.poke(0x79.U)
      c.io.in.valid.poke(true.B)
      receiveByte(c, 0x79)
    }
  }
}
