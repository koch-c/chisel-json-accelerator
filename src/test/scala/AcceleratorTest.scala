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

  /** The 12-byte NumberSerializer frame for one number. */
  private def numberFrame(
      id: Int,
      value: BigInt,
      negative: Boolean = false,
      isFloat: Boolean = false,
      exp10: Int = 0,
      notANumber: Boolean = false
  ): Seq[Int] = {
    val flags = (if (negative) 1 else 0) | (if (isFloat) 2
                                            else 0) | (if (notANumber) 8 else 0)
    Seq(id, flags) ++
      (0 until 8).map(i => ((value >> (8 * i)) & 0xff).toInt) ++
      (0 until 2).map(i => (exp10 >> (8 * i)) & 0xff)
  }

  /** The StringParser frame for one decoded string. */
  private def stringFrame(id: Int, text: String, flags: Int = 0): Seq[Int] =
    Seq(id) ++ text.getBytes("UTF-8").map(_ & 0xff) ++ Seq(0, flags)

  "Accelerator" should "extract numbers and reset between matching and missing records" in {
    run(JsonPathConfig(Seq(Seq("temperature")))) { d =>
      d.record("{\"temperature\": 25,\"humidity\":60}\n", ": 25")
      d.record("{\"humidity\":60}\n", "")
      d.record("{\"temperature\":-1.25e+2  }\n", ":-1.25e+2  ")
    }
  }

  it should "preserve string quotes, escapes and structural characters" in {
    run(JsonPathConfig(Seq(Seq("name")))) { d =>
      d.record(
        "{\"name\" : \"a\\\"b,}:\\n\\u0041\" }\n",
        ": \"a\\\"b,}:\\n\\u0041\" "
      )
    }
  }

  it should "select nested paths rather than matching keys elsewhere" in {
    run(JsonPathConfig(Seq(Seq("device", "location", "latitude")))) { d =>
      d.record(
        "{\"latitude\":0,\"device\":{\"location\":{\"latitude\": 12.34}}}\n",
        ": 12.34"
      )
    }
  }

  it should "concatenate repeated matches under output backpressure" in {
    run(JsonPathConfig(Seq(Seq("v"))), stall = true) { d =>
      d.record("{\"v\":1,\"v\":\"two\",\"v\":3}\n", ":1:\"two\":3")
    }
  }

  it should "preserve values across memory word boundaries" in {
    run(JsonPathConfig(Seq(Seq("text")))) { d =>
      d.record("{\"text\": \"0123456789ab\"}\n", ": \"0123456789ab\"")
    }
  }

  it should "extract before LF but wait for LF before completing the record" in {
    run(JsonPathConfig(Seq(Seq("v")))) { d =>
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
    run(JsonPathConfig(Seq(Seq("v")))) { d =>
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
    run(
      JsonPathConfig(
        Seq(Seq("temperature")),
        attributeId = 5,
        valueType = ValueType.Number
      )
    ) { d =>
      d.recordBytes(
        "{\"temperature\": 25,\"humidity\":60}\n",
        numberFrame(5, 25)
      )
      d.recordBytes("{\"humidity\":60}\n", Seq.empty)
      d.recordBytes(
        "{\"temperature\":-1.25e+2  }\n",
        numberFrame(5, 125, negative = true, isFloat = true, exp10 = 0)
      )
    }
  }

  it should "parse a number selected by a nested path" in {
    run(
      JsonPathConfig(
        Seq(Seq("device", "location", "latitude")),
        valueType = ValueType.Number
      )
    ) { d =>
      d.recordBytes(
        "{\"latitude\":0,\"device\":{\"location\":{\"latitude\": 12.34}}}\n",
        numberFrame(0, 1234, isFloat = true, exp10 = -2)
      )
    }
  }

  it should "send one frame per match under output backpressure" in {
    run(
      JsonPathConfig(Seq(Seq("v")), valueType = ValueType.Number),
      stall = true
    ) { d =>
      d.recordBytes(
        "{\"v\":1,\"v\":-2.5,\"v\":3e2}\n",
        numberFrame(0, 1) ++
          numberFrame(0, 25, negative = true, isFloat = true, exp10 = -1) ++
          numberFrame(0, 3, isFloat = true, exp10 = 2)
      )
      d.recordBytes("{\"v\":7}\n", numberFrame(0, 7))
    }
  }

  it should "flag a value that is not a number" in {
    run(JsonPathConfig(Seq(Seq("v")), valueType = ValueType.Number)) { d =>
      d.recordBytes("{\"v\": \"hot\"}\n", numberFrame(0, 0, notANumber = true))
      d.recordBytes("{\"v\":null}\n", numberFrame(0, 0, notANumber = true))
      d.recordBytes("{\"v\":21}\n", numberFrame(0, 21))
    }
  }

  it should "send Int32 values in two's complement and flag the ones that do not fit" in {
    run(
      JsonPathConfig(
        Seq(Seq("v")),
        attributeId = 2,
        valueType = ValueType.Int32
      )
    ) { d =>
      d.recordBytes(
        "{\"v\":25,\"v\":-7}\n",
        Seq(2, 0, 25, 0, 0, 0) ++ Seq(2, 1, 0xf9, 0xff, 0xff, 0xff)
      )
      d.recordBytes("{\"v\":2147483648}\n", Seq(2, 4, 0, 0, 0, 0x80))
      d.recordBytes("{\"v\":1.5}\n", Seq(2, 2, 1, 0, 0, 0))
      d.recordBytes("{\"v\":\"hot\"}\n", Seq(2, 8, 0, 0, 0, 0))
    }
  }

  it should "send Int64 values in two's complement" in {
    run(
      JsonPathConfig(Seq(Seq("v")), valueType = ValueType.Int64),
      stall = true
    ) { d =>
      d.recordBytes(
        "{\"v\":-9223372036854775808,\"v\":4294967296}\n",
        Seq(0, 1, 0, 0, 0, 0, 0, 0, 0, 0x80) ++ Seq(0, 0, 0, 0, 0, 0, 1, 0, 0,
          0)
      )
    }
  }

  it should "send a string as a decoded frame and nothing for a missing key" in {
    run(
      JsonPathConfig(
        Seq(Seq("name")),
        attributeId = 5,
        valueType = ValueType.String
      )
    ) { d =>
      d.recordBytes("{\"name\": \"Ada\",\"age\":36}\n", stringFrame(5, "Ada"))
      d.recordBytes("{\"age\":36}\n", Seq.empty)
      d.recordBytes("{\"name\":\"\"  }\n", stringFrame(5, ""))
    }
  }

  it should "decode escapes and keep structural characters in a string" in {
    run(JsonPathConfig(Seq(Seq("name")), valueType = ValueType.String)) { d =>
      d.recordBytes(
        "{\"name\" : \"a\\\"b,}:\\n\\\\ \\/\" }\n",
        stringFrame(0, "a\"b,}:\n\\ /")
      )
      // \u0041 is not decoded: the frame is flagged with badEscape.
      d.recordBytes(
        "{\"name\" : \"\\u0041\" }\n",
        stringFrame(0, "u0041", flags = 2)
      )
    }
  }

  it should "decode a string selected by a nested path" in {
    run(
      JsonPathConfig(
        Seq(Seq("device", "location", "city")),
        valueType = ValueType.String
      )
    ) { d =>
      d.recordBytes(
        "{\"city\":\"x\",\"device\":{\"location\":{\"city\": \"Kongens Lyngby\"}}}\n",
        stringFrame(0, "Kongens Lyngby")
      )
    }
  }

  it should "send one string frame per match under output backpressure" in {
    run(
      JsonPathConfig(Seq(Seq("v")), valueType = ValueType.String),
      stall = true
    ) { d =>
      d.recordBytes(
        "{\"v\":\"one\",\"v\":2,\"v\":\"0123456789abcdefghij\"}\n",
        stringFrame(0, "one") ++ stringFrame(0, "", flags = 1) ++
          stringFrame(0, "0123456789abcdefghij")
      )
      d.recordBytes("{\"v\":\"again\"}\n", stringFrame(0, "again"))
    }
  }

  it should "flag a value that is not a string" in {
    run(JsonPathConfig(Seq(Seq("v")), valueType = ValueType.String)) { d =>
      d.recordBytes("{\"v\": 21}\n", stringFrame(0, "", flags = 1))
      d.recordBytes("{\"v\":null}\n", stringFrame(0, "", flags = 1))
      d.recordBytes("{\"v\":\"hot\"}\n", stringFrame(0, "hot"))
    }
  }

  it should "give each path its own value type and attribute id" in {
    val config = JsonPathConfig(
      Seq(Seq("id"), Seq("user", "name")),
      attributeId = 4,
      valueTypes = Seq(ValueType.Int32, ValueType.String)
    )
    run(config) { d =>
      d.recordBytes(
        "{\"id\":7,\"user\":{\"name\":\"bob\",\"id\":9},\"tags\":[1,2]}\n",
        Seq(4, 0, 7, 0, 0, 0) ++ stringFrame(5, "bob")
      )
      // Document order, not config order.
      d.recordBytes(
        "{\"user\":{\"name\":\"Ada\"},\"id\":-2}\n",
        stringFrame(5, "Ada") ++ Seq(4, 1, 0xfe, 0xff, 0xff, 0xff)
      )
      d.recordBytes("{\"other\":1}\n", Seq.empty)
    }
  }

  it should "keep document order with several parsers per type" in {
    val config = JsonPathConfig(
      Seq(Seq("v")),
      valueType = ValueType.String,
      parsersPerType = 2
    )
    val long = "a first value that takes much longer to send than the next"
    run(config, stall = true) { d =>
      d.recordBytes(
        "{\"v\":\"" + long + "\",\"v\":\"b\",\"v\":3,\"v\":\"d\"}\n",
        stringFrame(0, long) ++ stringFrame(0, "b") ++
          stringFrame(0, "", flags = 1) ++ stringFrame(0, "d")
      )
      d.recordBytes("{\"v\":\"again\"}\n", stringFrame(0, "again"))
    }
  }

  it should "give the same output with one parser per type and with three" in {
    val random = new scala.util.Random(11)
    def value(): String =
      if (random.nextBoolean()) (random.nextInt(200000) - 100000).toString
      else
        "\"" + Seq
          .fill(random.nextInt(12))("abc xyz,}:".charAt(random.nextInt(10)))
          .mkString + "\""
    // Records with several matches close together, where the lanes overlap.
    val records = Seq.fill(12) {
      val fields = Seq.fill(1 + random.nextInt(5)) {
        val key = Seq("n", "s", "skip")(random.nextInt(3))
        "\"" + key + "\":" + " " * random.nextInt(3) + value()
      }
      "{" + fields.mkString(",") + "}\n"
    }
    def outputWith(parsers: Int): Seq[Int] = {
      var result = Seq.empty[Int]
      val config = JsonPathConfig(
        Seq(Seq("n"), Seq("s")),
        valueTypes = Seq(ValueType.Number, ValueType.String),
        parsersPerType = parsers
      )
      run(config, stall = parsers > 1) { d =>
        for (record <- records) {
          val previous = d.completions
          d.send(record)
          d.waitFor(d.completions > previous)
        }
        result = d.output.toSeq
      }
      result
    }
    val reference = outputWith(1)
    assert(reference.nonEmpty)
    assert(outputWith(3) == reference)
  }

  it should "match a software reference for random numbers" in {
    val random = new scala.util.Random(2026)
    def digits(count: Int): String =
      Seq.fill(count)(random.nextInt(10)).mkString
    // At most 18 mantissa digits, so every number fits in 64 bits.
    def randomNumber(): String = {
      val sign = if (random.nextBoolean()) "-" else ""
      val integer =
        if (random.nextInt(5) == 0) "0"
        else (1 + random.nextInt(9)).toString + digits(random.nextInt(9))
      val fraction =
        if (random.nextBoolean()) "." + digits(1 + random.nextInt(9)) else ""
      val exponent =
        if (random.nextInt(3) == 0)
          Seq("e", "E")(random.nextInt(2)) + Seq("", "+", "-")(
            random.nextInt(3)
          ) +
            digits(1 + random.nextInt(3))
        else ""
      sign + integer + fraction + exponent
    }

    run(JsonPathConfig(Seq(Seq("v")), valueType = ValueType.Number)) { d =>
      for (_ <- 0 until 40) {
        val text = randomNumber()
        // Random padding moves the number to a different byte offset each time.
        val pad = "x" * random.nextInt(9)
        val gap = " " * random.nextInt(3)
        val previous = d.completions
        d.send("{\"pad\":\"" + pad + "\",\"v\":" + gap + text + gap + "}\n")
        d.waitFor(d.completions > previous)

        val frame = d.output.toSeq
        d.output.clear()
        assert(frame.length == 12, s"Expected one frame for $text, got $frame")
        val flags = frame(1)
        val value = (0 until 8).map(i => BigInt(frame(2 + i)) << (8 * i)).sum
        val exp10 = (frame(10) | (frame(11) << 8)).toShort.toInt
        val negative = (flags & 1) != 0
        val isFloat = (flags & 2) != 0

        assert(negative == text.startsWith("-"), s"sign of $text")
        assert(
          isFloat == text.exists(c => c == '.' || c == 'e' || c == 'E'),
          s"isFloat of $text"
        )
        assert((flags & 12) == 0, s"overflow or notANumber set for $text")

        // Rebuild the value from the frame and compare it with the JVM's own parse.
        val magnitude = new java.math.BigDecimal(value.bigInteger)
          .scaleByPowerOfTen(if (isFloat) exp10 else 0)
        val expected = new java.math.BigDecimal(text).abs()
        assert(
          magnitude.compareTo(expected) == 0,
          s"$text was parsed as $magnitude"
        )
      }
    }
  }
}
