import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class StreamerThroughputSpec extends AnyFlatSpec with ChiselScalatestTester {
  // A clock assumption for reporting bandwidth, not a timing-closure measurement.
  private val clockHz = 100_000_000.0
  private val json = "{\"data\":\"" + ("abcdefgh" * 64) + "\"}"
  private val payload = json.padTo((json.length + 7) / 8 * 8, ' ')

  for (stallCycles <- Seq(0, 8, 16)) {
    "Streamer throughput" should s"measure transfer with $stallCycles stalled cycles per word" in {
      test(new Streamer).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        c.clock.setTimeout(10000)
        var sent = 0
        var received = 0
        var cycles = 0
        var waiting = 0
        val words = payload.length / 8
        val budget = words * (10 + stallCycles) + 20

        // Offer a byte every cycle; advance only when the Streamer accepts it.
        while (received < words && cycles < budget) {
          val inputValid = sent < payload.length
          c.io.in.valid.poke(inputValid.B)
          c.io.in.bits.poke((if (inputValid) payload(sent).toInt else 0).U)

          val outputValid = c.io.out.valid.peek().litToBoolean
          val outputReady = waiting >= stallCycles
          c.io.out.ready.poke(outputReady.B)
          val inputFire = inputValid && c.io.in.ready.peek().litToBoolean
          val outputFire = outputValid && outputReady

          if (outputFire) {
            val expected = payload.slice(received * 8, received * 8 + 8)
            for ((char, index) <- expected.zipWithIndex) {
              c.io.out.bits(index).expect(char.toInt.U)
            }
            received += 1
          }

          waiting = if (outputValid && !outputReady) waiting + 1 else 0
          if (inputFire) sent += 1
          c.clock.step()
          cycles += 1
        }

        assert(received == words, s"Transfer timed out after $cycles cycles")
        assert(sent == payload.length)
        // A queue lets collection overlap consumer stalls. Allow queue latency,
        // but require at least the throughput of the former unbuffered design.
        val unbufferedCycles = words * (9 + stallCycles)
        assert(cycles <= unbufferedCycles + 2,
          s"Expected overlapping collection and output, got $cycles cycles")
        val bytesPerCycle = payload.length.toDouble / cycles
        val megabytesPerSecond = bytesPerCycle * clockHz / 1e6
        println(f"Streamer: ${payload.length}%d bytes in $cycles%d cycles, " +
          f"$bytesPerCycle%.4f bytes/cycle, $megabytesPerSecond%.2f MB/s " +
          f"assuming ${clockHz / 1e6}%.0f MHz ($stallCycles%d stalled cycles/word)")
      }
    }
  }
}
