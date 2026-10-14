import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AcceleratorSpec extends AnyFlatSpec with ChiselScalatestTester {
  val key = "temperature"
  val debounceCycles = 8
  val matchingJson = """{ "temperature": 25, "humidity": 60 }"""
  val missingJson = """{ "humidity": 60 }"""

  // Each eight-character word needs a transfer cycle and eight parsing cycles.
  // Include extra time for debounce synchronization and result registration.
  def processingBudget(json: String): Int = ((json.length + 7) / 8) * 10 + 20

  def expectLeds(c: Accelerator, redOn: Boolean, greenOn: Boolean): Unit = {
    c.io.LED16_R.expect((redOn).B)
    c.io.LED16_G.expect((greenOn).B)
    c.io.LED16_B.expect(true.B)
  }

  "Accelerator" should "keep all LEDs off without a button press when the key is present" in {
    test(new Accelerator(key, matchingJson, debounceCycles)) { c =>
      c.io.button.poke(false.B)
      for (_ <- 0 until processingBudget(matchingJson)) {
        expectLeds(c, redOn = false, greenOn = false)
        c.clock.step()
      }
      expectLeds(c, redOn = false, greenOn = false)
    }
  }

  it should "keep all LEDs off without a button press when the key is missing" in {
    test(new Accelerator(key, missingJson, debounceCycles)) { c =>
      c.io.button.poke(false.B)
      for (_ <- 0 until processingBudget(missingJson)) {
        expectLeds(c, redOn = false, greenOn = false)
        c.clock.step()
      }
      expectLeds(c, redOn = false, greenOn = false)
    }
  }

  it should "ignore a button press shorter than the debounce interval" in {
    test(new Accelerator(key, matchingJson, debounceCycles)) { c =>
      c.io.button.poke(false.B)
      c.clock.step(4)
      c.io.button.poke(true.B)
      c.clock.step()
      expectLeds(c, redOn = false, greenOn = false)
      c.io.button.poke(false.B)
      for (_ <- 0 until processingBudget(matchingJson)) {
        c.clock.step()
        expectLeds(c, redOn = false, greenOn = false)
      }
    }
  }

  for ((json, found) <- Seq((matchingJson, true), (missingJson, false))) {
    val color = if (found) "green" else "red"
    it should s"show $color after a valid press and retain the result after release" in {
      test(new Accelerator(key, json, debounceCycles)) { c =>
        c.io.button.poke(false.B)
        c.clock.step(4)
        c.io.button.poke(true.B)
        c.clock.step(debounceCycles + 4)

        // Release after the start trigger, before the JSON has finished parsing.
        // Parsing must continue without the button being held down.
        c.io.button.poke(false.B)
        c.clock.step(processingBudget(json))
        println(s"LEDs: red ${c.io.LED16_R.peek()} and green: ${c.io.LED16_G.peek()} ")
        expectLeds(c, redOn = !found, greenOn = found)

        // The completed result must remain visible while the button is released.
        for (_ <- 0 until processingBudget(json)) {
          c.clock.step()
          expectLeds(c, redOn = !found, greenOn = found)
        }
      }
    }
  }
}
