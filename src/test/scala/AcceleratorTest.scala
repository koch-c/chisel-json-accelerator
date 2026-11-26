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
      val bytes = text.getBytes("UTF-8").map(_ & 0xff)
      for (batch <- bytes.grouped(8)) {
        c.io.in.valid.poke(true.B)
        c.io.in.bits.count.poke(batch.length.U)
        for (lane <- 0 until 8) {
          c.io.in.bits
            .bytes(lane)
            .poke(
              (if (lane < batch.length) batch(lane) else 0).U
            )
        }
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
      c.io.in.bits.count.poke(0.U)
      for (lane <- 0 until 8) c.io.in.bits.bytes(lane).poke(0.U)
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
        Seq(1, 29) ++ Seq.fill(8)(0) ++ Seq.fill(8)(255)
      )
      d.recordBytes(
        """{"s":"xy","s":"a","n":2147483648}""" + "\n",
        Seq(1, 7, 0, 0, 0, 0, 1, 0, 97, 0) ++ Seq.fill(8)(0)
      )
      d.recordBytes("""{"s":1}""" + "\n", Seq(1, 8) ++ Seq.fill(16)(0))
    }
  }
  it should "hold completion until LF and output acceptance" in {
    run(JsonPathConfig(Seq(FieldSpec(Seq("v"))))) { d =>
      d.allowOutput = false
      d.send("{\"v\": 1}")
      for (_ <- 0 until 80) d.tick()
      assert(d.completions == 0)
      d.allowOutput = true
      d.waitFor(d.output.length == 6)
      for (_ <- 0 until 10) d.tick()
      assert(d.completions == 0)
      d.recordBytes("\n", Seq(1, 1, 1, 0, 0, 0))
    }
  }

  it should "AND signed integer and exact string predicates" in {
    val n = Seq("n")
    val s = Seq("s")
    val config = JsonPathConfig(
      Seq(FieldSpec(n), FieldSpec(s, ValueType.String, 4)),
      filters = Seq(
        FilterSpec(n, CompareOp.Lt, IntLit(0)),
        FilterSpec(s, CompareOp.Eq, StrLit("Ada"))
      )
    )
    run(config, stall = true) { d =>
      val kept = Seq(1, 5, 255, 255, 255, 255, 3, 0, 65, 100, 97, 0)
      d.recordBytes("{\"s\":\"Ada\",\"n\":-1}\n", kept)
      d.recordBytes("{\"s\":\"Ada!\",\"n\":-1}\n", Seq(0))
      d.recordBytes("{\"s\":\"Ada\",\"n\":1}\n", Seq(0))
      d.recordBytes("{\"s\":\"Ada\"}\n", Seq(0))
      d.recordBytes("{\"s\":\"Ada\",\"n\":2147483648}\n", Seq(0))
      d.recordBytes("{\"s\":\"Ada\",\"n\":null}\n", Seq(0))
      d.recordBytes("{\"s\":\"Ada\",\"n\":-1}\n", kept)
    }
  }

  it should "match the FPGA profile for nested fields and filter boundaries" in {
    run(FpgaProfile.config, stall = true) { d =>
      for (request <- FpgaTestCases.requests) {
        withClue(s"${request.name}: ") {
          d.recordBytes(request.json + "\n", request.expected)
        }
      }
    }
  }

  it should "send a rejection byte between two kept records" in {
    val path = Seq("v")
    val config = JsonPathConfig(
      Seq(FieldSpec(path)),
      filters = Seq(FilterSpec(path, CompareOp.Gt, IntLit(0)))
    )
    run(config) { d =>
      d.recordBytes("{\"v\":1}\n", Seq(1, 1, 1, 0, 0, 0))
      d.recordBytes("{\"v\":0}\n", Seq(0))
      d.recordBytes("{\"v\":2}\n", Seq(1, 1, 2, 0, 0, 0))
    }
  }

  it should "send invalid or missing Int64 slots when there are no filters" in {
    val config = JsonPathConfig(Seq(FieldSpec(Seq("v"), ValueType.Int64)))
    run(config) { d =>
      d.recordBytes("{\"v\":-12.5}\n", Seq(1, 2) ++ Seq.fill(8)(0))
      d.recordBytes("{}\n", Seq(1, 0) ++ Seq.fill(8)(0))
    }
  }

  it should "wait for the rejection byte or final kept byte to be accepted" in {
    val path = Seq("v")
    val config = JsonPathConfig(
      Seq(FieldSpec(path)),
      filters = Seq(FilterSpec(path, CompareOp.Eq, IntLit(1)))
    )
    run(config) { d =>
      d.allowOutput = false
      d.send("{}\n")
      for (_ <- 0 until 30) d.tick()
      assert(d.completions == 0 && d.output.isEmpty)
      d.allowOutput = true
      d.waitFor(d.completions == 1)
      d.expectBytes(Seq(0))
      d.send("{\"v\":1}\n")
      d.waitFor(d.output.length == 5)
      d.allowOutput = false
      for (_ <- 0 until 20) d.tick()
      assert(d.completions == 1 && d.output.length == 5)
      d.allowOutput = true
      d.waitFor(d.completions == 2)
      d.expectBytes(Seq(1, 1, 1, 0, 0, 0))
    }
  }
}
