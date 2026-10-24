import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class UartSpec extends AnyFlatSpec with ChiselScalatestTester {
  for (cyclesPerBit <- Seq(8, 10)) {
    "Uart" should s"echo back-to-back bytes with $cyclesPerBit clocks per bit" in {
      test(new Uart(cyclesPerBit * 10, 10)) { c =>
        c.io.uartRx.poke(true.B)
        c.reset.poke(true.B)
        c.clock.step(2)
        c.reset.poke(false.B)
        for (_ <- 0 until cyclesPerBit * 2) {
          c.io.uartTx.expect(true.B)
          c.clock.step()
        }

        val bytes = Seq(0x96, 0x41, 0xff, 0x00, 0x55, 0xaa)
        def frame(byte: Int): Seq[Boolean] =
          Seq(false) ++ (0 until 8).map(bit => ((byte >> bit) & 1) != 0) ++ Seq(true)

        fork {
          for (byte <- bytes; bit <- frame(byte)) {
            c.io.uartRx.poke(bit.B)
            c.clock.step(cyclesPerBit)
          }
          // Keep the input idle while the final echoed frame finishes.
          c.io.uartRx.poke(true.B)
          c.clock.step(12 * cyclesPerBit)
        }.fork {
          for (byte <- bytes) {
            var waited = 0
            while (c.io.uartTx.peek().litToBoolean && waited < 12 * cyclesPerBit) {
              c.clock.step()
              waited += 1
            }
            assert(waited < 12 * cyclesPerBit, "Timed out waiting for echoed start bit")
            // Check every cycle, not just bit midpoints, to verify TX duration.
            for (bit <- frame(byte); _ <- 0 until cyclesPerBit) {
              c.io.uartTx.expect(bit.B)
              c.clock.step()
            }
          }
          for (_ <- 0 until cyclesPerBit) {
            c.io.uartTx.expect(true.B)
            c.clock.step()
          }
        }.join()
      }
    }
  }
}
