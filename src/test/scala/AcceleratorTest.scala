import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AcceleratorSpec extends AnyFlatSpec with ChiselScalatestTester {
  val key = "temperature"

  def initialize(c: Accelerator): Unit = {
    c.clock.setTimeout(0)
    c.io.uartRx.poke(true.B)
    c.reset.poke(true.B)
    c.clock.step(2)
    c.reset.poke(false.B)
    c.clock.step(4)
  }

  def send(c: Accelerator, text: String, cyclesPerBit: Int): Unit = {
    for (char <- text) {
      val frame = Seq(false) ++ (0 until 8).map(bit => ((char.toInt >> bit) & 1) != 0) ++ Seq(true)
      for (bit <- frame) {
        c.io.uartRx.poke(bit.B)
        c.clock.step(cyclesPerBit)
      }
    }
    c.io.uartRx.poke(true.B)
  }

  def receive(c: Accelerator, expected: String, cyclesPerBit: Int, budget: Int): Unit = {
    for (char <- expected) {
      var waited = 0
      while (c.io.uartTx.peek().litToBoolean && waited < budget) {
        c.clock.step()
        waited += 1
      }
      assert(waited < budget, s"Timed out waiting for response character $char")
      val frame = Seq(false) ++ (0 until 8).map(bit => ((char.toInt >> bit) & 1) != 0) ++ Seq(true)
      for (bit <- frame; _ <- 0 until cyclesPerBit) {
        c.io.uartTx.expect(bit.B)
        c.clock.step()
      }
    }
  }

  def expectLeds(c: Accelerator, found: Boolean): Unit = {
    c.io.LED16_R.expect((!found).B)
    c.io.LED16_G.expect(found.B)
    c.io.LED16_B.expect(false.B)
  }

  def expectQuiet(c: Accelerator, cycles: Int): Unit = {
    for (_ <- 0 until cycles) {
      c.io.uartTx.expect(true.B)
      c.clock.step()
    }
  }

  val documents = Seq(
    ("matching key", """{ "temperature": 25, "humidity": 60 }""", true, "25\nFOUND\n"),
    ("missing key", """{ "humidity": 60 }""", false, "NOT_FOUND\n"),
    ("long matching input", """{"padding":"""" + ("x" * 257) + """","temperature":25}""", true, "25\nFOUND\n"),
    ("long missing input", """{"padding":"""" + ("x" * 257) + """","humidity":60}""", false, "NOT_FOUND\n"),
    ("nested string value", """{"meta":{"temperature":"twenty-five"}}""", true, "\"twenty-five\"\nFOUND\n")
  )

  for (cyclesPerBit <- Seq(8, 10); (label, json, found, response) <- documents) {
    "Accelerator" should s"report $label exactly once with $cyclesPerBit clocks per bit" in {
      test(new Accelerator(key, cyclesPerBit * 10, 10)) { c =>
        initialize(c)
        c.io.LED16_R.expect(false.B)
        c.io.LED16_G.expect(false.B)
        expectQuiet(c, 20)
        val padded = json.padTo((json.length + 7) / 8 * 8, ' ')
        fork {
          send(c, padded, cyclesPerBit)
        }.fork {
          receive(c, response, cyclesPerBit,
            (padded.length + 12) * 10 * cyclesPerBit)
        }.join()
        c.clock.step()
        expectLeds(c, found)
        // More input must not retrigger a response or replace the latched result.
        fork {
          send(c, "{}      ", cyclesPerBit)
        }.fork {
          expectQuiet(c, 100 * cyclesPerBit)
        }.join()
        expectLeds(c, found)
      }
    }
  }

  it should "wait for final-block padding and accept a new document after reset" in {
    val cyclesPerBit = 8
    test(new Accelerator(key, cyclesPerBit * 10, 10)) { c =>
      initialize(c)
      send(c, "{}", cyclesPerBit)
      expectQuiet(c, 100)
      c.io.LED16_R.expect(false.B)
      c.io.LED16_G.expect(false.B)
      fork {
        send(c, "      ", cyclesPerBit)
      }.fork {
        receive(c, "NOT_FOUND\n", cyclesPerBit, 1000)
      }.join()
      expectLeds(c, found = false)
      initialize(c)
      c.io.LED16_R.expect(false.B)
      c.io.LED16_G.expect(false.B)
      val json = """{"temperature":1}"""
      fork {
        send(c, json.padTo((json.length + 7) / 8 * 8, ' '), cyclesPerBit)
      }.fork {
        receive(c, "1\nFOUND\n", cyclesPerBit, 3000)
      }.join()
      expectLeds(c, found = true)
      expectQuiet(c, 1000)
    }
  }
}
