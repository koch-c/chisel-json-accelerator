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

  private def run(config: JsonPathConfig, stall: Boolean = false)(body: Driver => Unit): Unit = {
    test(new Accelerator(config)) { c =>
      c.io.in.valid.poke(false.B)
      c.io.in.bits.poke(0.U)
      c.io.out.ready.poke(true.B)
      body(new Driver(c, stall))
    }
  }

  /** The 12-byte NumberSerializer frame for one number. */
  private def numberFrame(id: Int, value: BigInt, negative: Boolean = false,
                          isFloat: Boolean = false, exp10: Int = 0,
                          notANumber: Boolean = false): Seq[Int] = {
    val flags = (if (negative) 1 else 0) | (if (isFloat) 2 else 0) | (if (notANumber) 8 else 0)
    Seq(id, flags) ++
      (0 until 8).map(i => ((value >> (8 * i)) & 0xff).toInt) ++
      (0 until 2).map(i => (exp10 >> (8 * i)) & 0xff)
  }

  "Accelerator" should "extract numbers and reset between matching and missing records" in {
    run(JsonPathConfig(Seq("temperature"))) { d =>
      d.record("{\"temperature\": 25,\"humidity\":60}\n", ": 25")
      d.record("{\"humidity\":60}\n", "")
      d.record("{\"temperature\":-1.25e+2  }\n", ":-1.25e+2  ")
    }
  }

  it should "preserve string quotes, escapes and structural characters" in {
    run(JsonPathConfig(Seq("name"))) { d =>
      d.record("{\"name\" : \"a\\\"b,}:\\n\\u0041\" }\n",
        ": \"a\\\"b,}:\\n\\u0041\" ")
    }
  }

  it should "select nested paths rather than matching keys elsewhere" in {
    run(JsonPathConfig(Seq("device", "location", "latitude"))) { d =>
      d.record("{\"latitude\":0,\"device\":{\"location\":{\"latitude\": 12.34}}}\n", ": 12.34")
    }
  }

  it should "concatenate repeated matches under output backpressure" in {
    run(JsonPathConfig(Seq("v")), stall = true) { d =>
      d.record("{\"v\":1,\"v\":\"two\",\"v\":3}\n", ":1:\"two\":3")
    }
  }

  it should "preserve values across memory word boundaries" in {
    run(JsonPathConfig(Seq("text"))) { d =>
      d.record("{\"text\": \"0123456789ab\"}\n", ": \"0123456789ab\"")
    }
  }

  it should "extract before LF but wait for LF before completing the record" in {
    run(JsonPathConfig(Seq("v"))) { d =>
      d.send("{\"v\": 1,") // Full word; extraction can start immediately.
      d.waitFor(d.output.length == 3)
      d.expect(": 1")
      assert(d.completions == 0)
      d.send("\"v\":2}\n")
      d.waitFor(d.completions == 1)
      d.expect(":2")
      d.send("{\"v\": 3}") // Root closes in a full word, before LF.
      d.waitFor(d.output.length == 3)
      for (_ <- 0 until 10) d.tick()
      d.expect(": 3")
      assert(d.completions == 1, "Record completed before LF")
      d.send("\n")
      d.waitFor(d.completions == 2)
      d.record("{\"v\":4}\n", ":4")
    }
  }

  it should "wait for the consumer to accept output before asserting done" in {
    run(JsonPathConfig(Seq("v"))) { d =>
      d.allowOutput = false
      d.send("{\"v\":1}\n")
      for (_ <- 0 until 80) d.tick()
      assert(d.completions == 0)
      assert(d.output.isEmpty)
      d.allowOutput = true
      d.waitFor(d.completions == 1)
      d.expect(":1")
    }
  }

  it should "send a number as a binary frame and nothing for a missing key" in {
    run(JsonPathConfig(Seq("temperature"), attributeId = 5, valueType = ValueType.Number)) { d =>
      d.recordBytes("{\"temperature\": 25,\"humidity\":60}\n", numberFrame(5, 25))
      d.recordBytes("{\"humidity\":60}\n", Seq.empty)
      d.recordBytes("{\"temperature\":-1.25e+2  }\n",
        numberFrame(5, 125, negative = true, isFloat = true, exp10 = 0))
    }
  }

  it should "parse a number selected by a nested path" in {
    run(JsonPathConfig(Seq("device", "location", "latitude"), valueType = ValueType.Number)) { d =>
      d.recordBytes("{\"latitude\":0,\"device\":{\"location\":{\"latitude\": 12.34}}}\n",
        numberFrame(0, 1234, isFloat = true, exp10 = -2))
    }
  }

  it should "send one frame per match under output backpressure" in {
    run(JsonPathConfig(Seq("v"), valueType = ValueType.Number), stall = true) { d =>
      d.recordBytes("{\"v\":1,\"v\":-2.5,\"v\":3e2}\n",
        numberFrame(0, 1) ++
          numberFrame(0, 25, negative = true, isFloat = true, exp10 = -1) ++
          numberFrame(0, 3, isFloat = true, exp10 = 2))
      d.recordBytes("{\"v\":7}\n", numberFrame(0, 7))
    }
  }

  it should "flag a value that is not a number" in {
    run(JsonPathConfig(Seq("v"), valueType = ValueType.Number)) { d =>
      d.recordBytes("{\"v\": \"hot\"}\n", numberFrame(0, 0, notANumber = true))
      d.recordBytes("{\"v\":null}\n", numberFrame(0, 0, notANumber = true))
      d.recordBytes("{\"v\":21}\n", numberFrame(0, 21))
    }
  }

  it should "send Int32 values in two's complement and flag the ones that do not fit" in {
    run(JsonPathConfig(Seq("v"), attributeId = 2, valueType = ValueType.Int32)) { d =>
      d.recordBytes("{\"v\":25,\"v\":-7}\n", Seq(2, 0, 25, 0, 0, 0) ++ Seq(2, 1, 0xf9, 0xff, 0xff, 0xff))
      d.recordBytes("{\"v\":2147483648}\n", Seq(2, 4, 0, 0, 0, 0x80))
      d.recordBytes("{\"v\":1.5}\n", Seq(2, 2, 1, 0, 0, 0))
      d.recordBytes("{\"v\":\"hot\"}\n", Seq(2, 8, 0, 0, 0, 0))
    }
  }

  it should "send Int64 values in two's complement" in {
    run(JsonPathConfig(Seq("v"), valueType = ValueType.Int64), stall = true) { d =>
      d.recordBytes("{\"v\":-9223372036854775808,\"v\":4294967296}\n",
        Seq(0, 1, 0, 0, 0, 0, 0, 0, 0, 0x80) ++ Seq(0, 0, 0, 0, 0, 0, 1, 0, 0, 0))
    }
  }
}
