import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/** UART smoke tests for fixed-layout records; detailed extraction tests use
  * Accelerator directly.
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

      // The host waits for the whole response before starting the next line.
      for (
        (line, response) <- Seq(
          json -> expected,
          "{}\n" -> Seq.fill(config.recordBytes)(0)
        )
      ) {
        received.clear()
        for (b <- line.getBytes("UTF-8")) {
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
        val timeout = 200 + response.length * 20 * clocksPerBit
        while (received.length < response.length && waited < timeout) {
          tick()
          waited += 1
        }
        for (_ <- 0 until 2 * 10 * clocksPerBit) tick()
        assert(
          received.toSeq == response,
          s"Unexpected UART response: ${received.toSeq}"
        )
      }
    }
  }

  "AcceleratorTop" should "connect UART RX through the core to unmodified UART TX" in {
    exchange(
      JsonPathConfig(Seq(FieldSpec(Seq("v")))),
      "{\"v\":1}\n",
      Seq(1, 1, 0, 0, 0)
    )
  }

  it should "send an invalid integer record over UART" in {
    // Fractional syntax is invalid for an Int64 slot.
    exchange(
      JsonPathConfig(Seq(FieldSpec(Seq("v"), ValueType.Int64))),
      "{\"v\":-12.5}\n",
      Seq(2) ++ Seq.fill(8)(0)
    )
  }
}
