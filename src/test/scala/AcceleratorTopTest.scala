import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/** One serial smoke test; detailed extraction tests use Accelerator directly. */
class AcceleratorTopSpec extends AnyFlatSpec with ChiselScalatestTester {
  "AcceleratorTop" should "connect UART RX through the core to unmodified UART TX" in {
    val clocksPerBit = 8
    test(new AcceleratorTop(JsonPathConfig(Seq("v")), clockHz = 80, baudRate = 10)) { c =>
      c.io.uartRx.poke(true.B)
      c.clock.step(20)
      val received = scala.collection.mutable.ArrayBuffer.empty[Int]
      var receiving = false
      var elapsed = 0
      var byte = 0

      // Monitor TX during both input transmission and output draining.
      def tick(): Unit = {
        val tx = c.io.uartTx.peek().litToBoolean
        if (!receiving && !tx) {
          receiving = true
          elapsed = 0
          byte = 0
        }
        if (receiving) {
          for (bit <- 0 until 8) {
            if (elapsed == (bit + 1) * clocksPerBit + clocksPerBit / 2 && tx)
              byte |= 1 << bit
          }
          if (elapsed == 9 * clocksPerBit + clocksPerBit / 2) {
            assert(tx, "Invalid UART stop bit")
            received += byte
            receiving = false
          }
          elapsed += 1
        }
        c.clock.step()
      }

      for (b <- "{\"v\":1}\n".getBytes("UTF-8")) {
        val bits = Seq(false) ++ (0 until 8).map(i => ((b & 0xff) & (1 << i)) != 0) ++ Seq(true)
        for (bit <- bits) {
          c.io.uartRx.poke(bit.B)
          for (_ <- 0 until clocksPerBit) tick()
        }
      }
      c.io.uartRx.poke(true.B)
      var waited = 0
      while (received.length < 2 && waited < 1000) {
        tick()
        waited += 1
      }
      for (_ <- 0 until 2 * 10 * clocksPerBit) tick()
      assert(received.toSeq == Seq(':'.toInt, '1'.toInt),
        s"Unexpected UART response: ${received.toSeq}")
    }
  }
}
