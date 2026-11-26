import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AcceleratorSpec extends AnyFlatSpec with ChiselScalatestTester {
  private class Driver(c: Accelerator, stall: Boolean) {
    val output = scala.collection.mutable.ArrayBuffer.empty[Int]
    var completions = 0
    var allowOutput = true
    private var cycles = 0
    private var heldByte: Option[Int] = None

    def tick(): Unit = {
      val ready = allowOutput && (!stall || cycles % 5 == 0)
      c.io.out.ready.poke(ready.B)
      val valid = c.io.out.valid.peek().litToBoolean
      heldByte.foreach { byte =>
        assert(valid, "Output valid dropped while stalled")
        c.io.out.bits.expect(byte.U)
      }
      if (valid) {
        val byte = c.io.out.bits.peek().litValue.toInt
        if (ready) output += byte
        heldByte = if (ready) None else Some(byte)
      } else heldByte = None
      if (c.io.done.peek().litToBoolean) completions += 1
      c.clock.step()
      cycles += 1
    }

    def send(text: String): Unit = {
      for (byte <- text.getBytes("UTF-8")) {
        c.io.in.valid.poke(true.B)
        c.io.in.bits.poke((byte & 0xff).U)
        var waited = 0
        while (!c.io.in.ready.peek().litToBoolean && waited < 2000) {
          tick()
          waited += 1
        }
        assert(waited < 2000, "Input remained stalled")
        tick()
        c.io.in.valid.poke(false.B)
      }
    }

    def waitFor(condition: => Boolean): Unit = {
      var waited = 0
      while (!condition && waited < 2000) {
        tick()
        waited += 1
      }
      assert(condition, "Timed out waiting for extraction")
    }

    def expect(text: String): Unit =
      expectBytes(text.getBytes("UTF-8").map(_ & 0xff).toSeq)

    def expectBytes(bytes: Seq[Int]): Unit = {
      assert(output.toSeq == bytes, s"Unexpected raw output: ${output.toSeq}")
      output.clear()
    }

    def record(json: String, expected: String): Unit =
      recordBytes(json, expected.getBytes("UTF-8").map(_ & 0xff).toSeq)

    def recordBytes(json: String, expected: Seq[Int]): Unit = {
      val previous = completions
      send(json)
      waitFor(completions > previous)
      for (_ <- 0 until 4) tick()
      assert(completions == previous + 1, "Expected exactly one done pulse")
      expectBytes(expected)
    }
  }

  private def run(config: JsonPathConfig, stall: Boolean = false)(
      body: Driver => Unit
  ): Unit = {
    test(new Accelerator(config)) { c =>
      c.io.in.valid.poke(false.B)
      c.io.in.bits.poke(0.U)
      c.io.out.ready.poke(true.B)
      body(new Driver(c, stall))
    }
  }

  "Accelerator" should "assemble mixed fields in config order under stalls" in {
    val cfg = JsonPathConfig(
      Seq(
        FieldSpec(Seq("n")),
        FieldSpec(Seq("s"), ValueType.String, 2),
        FieldSpec(Seq("w"), ValueType.Int64)
      ),
      numericParsers = 2
    )
    run(cfg, stall = true) { d =>
      d.recordBytes(
        """{"w":-1,"s":"abc","n":0}""" + "\n",
        Seq(29) ++ Seq.fill(8)(0) ++ Seq.fill(8)(255)
      )
      d.recordBytes(
        """{"s":"xy","s":"a","n":2147483648}""" + "\n",
        Seq(7, 0, 0, 0, 0, 1, 0, 97, 0) ++ Seq.fill(8)(0)
      )
      d.recordBytes("""{"s":1}""" + "\n", Seq(8) ++ Seq.fill(16)(0))
    }
  }
  it should "hold completion until LF and output acceptance" in {
    run(JsonPathConfig(Seq(FieldSpec(Seq("v"))))) { d =>
      d.allowOutput = false
      d.send("{\"v\": 1}")
      for (_ <- 0 until 80) d.tick()
      assert(d.completions == 0)
      d.allowOutput = true
      d.waitFor(d.output.length == 5)
      for (_ <- 0 until 10) d.tick()
      assert(d.completions == 0)
      d.recordBytes("\n", Seq(1, 1, 0, 0, 0))
    }
  }
}
