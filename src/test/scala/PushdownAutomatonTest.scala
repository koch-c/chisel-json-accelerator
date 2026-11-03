import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class PushdownAutomatonSpec extends AnyFlatSpec with ChiselScalatestTester {
  private def send(c: PushdownAutomaton, text: String): Unit = {
    for (char <- text) {
      c.io.in.valid.poke(true.B)
      c.io.in.bits.poke(char.toInt.U)
      c.io.in.ready.expect(true.B)
      c.clock.step()
    }
    c.io.in.valid.poke(false.B)
  }

  "PushdownAutomaton" should "find a key at the requested nested path" in {
    test(new PushdownAutomaton(Seq("device", "location", "latitude"))) { c =>
      send(c, """{"device":{"location":{"latitude":55.7}}}""")
      c.io.found.expect(true.B)
      c.io.depth.expect(0.U)
    }
  }

  it should "not match the same key outside the requested path" in {
    test(new PushdownAutomaton(Seq("device", "location", "latitude"))) { c =>
      send(c, """{"other":{"latitude":55.7},"device":{"location":{}}}""")
      c.io.found.expect(false.B)
      c.io.depth.expect(0.U)
    }
  }

  it should "recognize a one-key path in the root object" in {
    test(new PushdownAutomaton(Seq("temperature"))) { c =>
      send(c, """{"temperature":25}""")
      c.io.found.expect(true.B)
    }
  }

  it should "keep scanning when an unrelated nested object is encountered" in {
    test(new PushdownAutomaton(Seq("device", "location", "latitude"))) { c =>
      send(c, """{"unrelated":{"latitude":1},"device":{"location":{"latitude":2}}}""")
      c.io.found.expect(true.B)
      c.io.depth.expect(0.U)
    }
  }
}
