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
        c.io.in.valid.expect(false.B)
        c.clock.step()
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
  //
  // it should "reject a short false start and an invalid stop bit" in {
  //   test(new UartTx(80, 10)) { c =>
  //     initialize(c)
  //     c.io.rx.poke(false.B)
  //     c.clock.step(2)
  //     c.io.rx.poke(true.B)
  //     for (_ <- 0 until 20) {
  //       c.io.out.valid.expect(false.B)
  //       c.clock.step()
  //     }
  //     sendByte(c, 0x96, validStop = false)
  //     c.io.out.valid.expect(false.B)
  //     c.io.rx.poke(true.B)
  //     c.clock.step(cyclesPerBit)
  //     sendByte(c, 0x41)
  //     c.io.out.valid.expect(true.B)
  //     c.io.out.bits.expect(0x41.U)
  //   }
  // }
  //
  // it should "receive back-to-back frames with one stop bit" in {
  //   test(new UartTx(80, 10)) { c =>
  //     initialize(c)
  //     val bytes = Seq(0x96, 0x41, 0xff, 0x00)
  //     val received = scala.collection.mutable.ArrayBuffer.empty[Int]
  //     c.io.out.ready.poke(true.B)
  //     fork {
  //       bytes.foreach(byte => sendByte(c, byte))
  //       c.clock.step(8)
  //     }.fork {
  //       for (_ <- 0 until bytes.length * 10 * cyclesPerBit + 8) {
  //         if (c.io.out.valid.peek().litToBoolean) {
  //           received += c.io.out.bits.peek().litValue.toInt
  //         }
  //         c.clock.step()
  //       }
  //     }.join()
  //     assert(received.toSeq == bytes)
  //   }
  // }
  //
  // it should "preserve a pending byte when another frame arrives" in {
  //   test(new UartTx(80, 10)) { c =>
  //     initialize(c)
  //     sendByte(c, 0x41)
  //     sendByte(c, 0x96)
  //     c.io.out.valid.expect(true.B)
  //     c.io.out.bits.expect(0x41.U)
  //   }
  // }
}
