import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class UartRxSpec extends AnyFlatSpec with ChiselScalatestTester {
  val cyclesPerBit = 8

  def initialize(c: UartRx): Unit = {
    c.io.rx.poke(true.B)
    c.io.out.ready.poke(false.B)
    c.reset.poke(true.B)
    c.clock.step(2)
    c.reset.poke(false.B)
    c.clock.step(cyclesPerBit)
  }

  def sendByte(c: UartRx, byte: Int, validStop: Boolean = true): Unit = {
    c.io.rx.poke(false.B)
    c.clock.step(cyclesPerBit)
    for (bit <- 0 until 8) {
      c.io.rx.poke((((byte >> bit) & 1) != 0).B)
      c.clock.step(cyclesPerBit)
    }
    c.io.rx.poke(validStop.B)
    c.clock.step(cyclesPerBit)
  }

  "UartRx" should "stay invalid while the line is idle" in {
    test(new UartRx(80, 10)) { c =>
      initialize(c)
      for (_ <- 0 until 100) {
        c.io.out.valid.expect(false.B)
        c.clock.step()
      }
    }
  }

  it should "receive LSB-first bytes and hold them until accepted" in {
    test(new UartRx(80, 10)) { c =>
      initialize(c)
      for (byte <- Seq(0x41, 0x96, 0x00, 0xff, 0x55, 0xaa)) {
        sendByte(c, byte)
        for (_ <- 0 until 12) {
          c.io.out.valid.expect(true.B)
          c.io.out.bits.expect(byte.U)
          c.clock.step()
        }
        c.io.out.ready.poke(true.B)
        c.clock.step()
        c.io.out.valid.expect(false.B)
        c.io.out.ready.poke(false.B)
      }
    }
  }

  it should "reject a short false start and an invalid stop bit" in {
    test(new UartRx(80, 10)) { c =>
      initialize(c)
      c.io.rx.poke(false.B)
      c.clock.step(2)
      c.io.rx.poke(true.B)
      for (_ <- 0 until 20) {
        c.io.out.valid.expect(false.B)
        c.clock.step()
      }
      sendByte(c, 0x96, validStop = false)
      c.io.out.valid.expect(false.B)
      c.io.rx.poke(true.B)
      c.clock.step(cyclesPerBit)
      sendByte(c, 0x41)
      c.io.out.valid.expect(true.B)
      c.io.out.bits.expect(0x41.U)
    }
  }

  it should "receive back-to-back frames with one stop bit" in {
    test(new UartRx(80, 10)) { c =>
      initialize(c)
      val bytes = Seq(0x96, 0x41, 0xff, 0x00)
      val received = scala.collection.mutable.ArrayBuffer.empty[Int]
      c.io.out.ready.poke(true.B)
      fork {
        bytes.foreach(byte => sendByte(c, byte))
        c.clock.step(8)
      }.fork {
        for (_ <- 0 until bytes.length * 10 * cyclesPerBit + 8) {
          if (c.io.out.valid.peek().litToBoolean) {
            received += c.io.out.bits.peek().litValue.toInt
          }
          c.clock.step()
        }
      }.join()
      assert(received.toSeq == bytes)
    }
  }

  it should "preserve a pending byte when another frame arrives" in {
    test(new UartRx(80, 10)) { c =>
      initialize(c)
      sendByte(c, 0x41)
      sendByte(c, 0x96)
      c.io.out.valid.expect(true.B)
      c.io.out.bits.expect(0x41.U)
    }
  }
}
