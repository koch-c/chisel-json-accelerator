import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/** One serial smoke test; detailed extraction tests use Accelerator directly.
  */
class AcceleratorTopSpec extends AnyFlatSpec with ChiselScalatestTester {

  /** Sends a record over the RX pin and checks the bytes that come back on TX.
    */
  private def exchange(
      config: JsonPathConfig,
      json: String,
      expected: Seq[Int]
  ): Unit = {
    val clocksPerBit = 8
    test(new AcceleratorTop(config, clockHz = 80, baudRate = 10)) { c =>
      // A frame takes longer to send than chiseltest's default limit of 1000
      // cycles without input. The wait loop below has its own timeout.
      c.clock.setTimeout(0)
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

      for (b <- json.getBytes("UTF-8")) {
        val bits = Seq(false) ++ (0 until 8).map(i =>
          ((b & 0xff) & (1 << i)) != 0
        ) ++ Seq(true)
        for (bit <- bits) {
          c.io.uartRx.poke(bit.B)
          for (_ <- 0 until clocksPerBit) tick()
        }
      }
      c.io.uartRx.poke(true.B)
      var waited = 0
      val timeout = 200 + expected.length * 20 * clocksPerBit
      while (received.length < expected.length && waited < timeout) {
        tick()
        waited += 1
      }
      for (_ <- 0 until 2 * 10 * clocksPerBit) tick()
      assert(
        received.toSeq == expected,
        s"Unexpected UART response: ${received.toSeq}"
      )
    }
  }

  "AcceleratorTop" should "connect UART RX through the core to unmodified UART TX" in {
    exchange(JsonPathConfig(Seq("v")), "{\"v\":1}\n", Seq(':'.toInt, '1'.toInt))
  }

  it should "send a number frame over UART" in {
    // -12.5 is mantissa 125 with exp10 -1. Flags: negative and isFloat.
    exchange(
      JsonPathConfig(Seq("v"), attributeId = 7, valueType = ValueType.Number),
      "{\"v\":-12.5}\n",
      Seq(7, 3, 125, 0, 0, 0, 0, 0, 0, 0, 0xff, 0xff)
    )
  }
}
