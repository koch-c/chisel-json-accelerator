import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class DebouncerSpec extends AnyFlatSpec with ChiselScalatestTester {
  "Debouncer" should "emit one pulse after a button remains pressed for the configured count" in {
    test(new Debouncer(count = 3)) { c =>
      c.io.in.poke(false.B)
      c.clock.step()
      c.io.fire.expect(false.B)

      c.io.in.poke(true.B)
      c.clock.step()
      c.io.fire.expect(false.B)

      c.clock.step(4)
      c.io.fire.expect(true.B)

      c.clock.step()
      c.io.fire.expect(false.B)
    }
  }

  it should "restart the debounce delay after the button is released" in {
    test(new Debouncer(count = 3)) { c =>
      c.io.in.poke(true.B)
      c.clock.step(5)
      c.io.fire.expect(true.B)

      c.io.in.poke(false.B)
      c.clock.step()
      c.io.fire.expect(false.B)

      c.io.in.poke(true.B)
      c.clock.step()
      c.clock.step(4)
      c.io.fire.expect(true.B)
    }
  }

  it should "not fire when the button is not pressed anymore" in {
    test(new Debouncer(count = 3)) { c =>
      c.io.in.poke(true.B)
      c.clock.step(6)
      c.io.in.poke(false.B)

      for (i <- 0 until 10) {
        c.clock.step(1)
        c.io.fire.expect(false.B)
      }
    }
  }
}
