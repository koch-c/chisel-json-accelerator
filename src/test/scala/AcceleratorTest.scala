import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AcceleratorSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val cyclesPerBit = 8

  private class Serial(c: Accelerator) {
    val output = scala.collection.mutable.ArrayBuffer.empty[Int]
    private var receiving = false
    private var elapsed = 0
    private var byte = 0

    def tick(): Unit = {
      val tx = c.io.uartTx.peek().litToBoolean
      if (!receiving && !tx) {
        receiving = true
        elapsed = 0
        byte = 0
      }
      if (receiving) {
        for (bit <- 0 until 8) {
          if (elapsed == (bit + 1) * cyclesPerBit + cyclesPerBit / 2 && tx)
            byte |= 1 << bit
        }
        if (elapsed == 9 * cyclesPerBit + cyclesPerBit / 2) {
          assert(tx, "Invalid UART stop bit")
          output += byte
          receiving = false
        }
        elapsed += 1
      }
      c.clock.step()
    }

    def send(text: String): Unit = {
      for (b <- text.getBytes("UTF-8")) {
        val bits = Seq(false) ++ (0 until 8).map(i => ((b & 0xff) & (1 << i)) != 0) ++ Seq(true)
        for (bit <- bits) {
          c.io.uartRx.poke(bit.B)
          for (_ <- 0 until cyclesPerBit) tick()
        }
      }
      c.io.uartRx.poke(true.B)
    }

    def expect(text: String): Unit = {
      val expected = text.getBytes("UTF-8").map(_ & 0xff).toSeq
      var waited = 0
      while (output.length < expected.length && waited < 20000) {
        tick()
        waited += 1
      }
      // Include a quiet interval to catch extra bytes and let the record reset.
      for (_ <- 0 until 1000) tick()
      assert(output.toSeq == expected, s"Expected $expected, received ${output.toSeq}")
      output.clear()
    }
  }

  private def run(config: JsonPathConfig)(body: Serial => Unit): Unit = {
    test(new Accelerator(config, clockHz = 80, baudRate = 10)) { c =>
      c.clock.setTimeout(0)
      c.io.uartRx.poke(true.B)
      c.reset.poke(true.B)
      c.clock.step(2)
      c.reset.poke(false.B)
      c.clock.step(20)
      body(new Serial(c))
    }
  }

  "Accelerator" should "transmit raw numeric ranges and reset between records" in {
    run(JsonPathConfig(Seq("temperature"))) { serial =>
      serial.send("{\"temperature\": 25, \"humidity\":60}\n")
      serial.expect(": 25")
      serial.send("{\"humidity\":60}\n")
      serial.expect("")
      serial.send("{\"temperature\":-1.25e+2  }\n")
      serial.expect(":-1.25e+2  ")
    }
  }

  it should "preserve quotes, escapes and structural characters inside strings" in {
    run(JsonPathConfig(Seq("name"))) { serial =>
      serial.send("{\"name\" : \"a\\\"b,}:\\n\\u0041\" }\n")
      serial.expect(": \"a\\\"b,}:\\n\\u0041\" ")
    }
  }

  it should "select a nested path rather than a matching key elsewhere" in {
    run(JsonPathConfig(Seq("device", "location", "latitude"))) { serial =>
      serial.send("{\"latitude\":0,\"device\":{\"location\":{\"latitude\": 12.34}}}\n")
      serial.expect(": 12.34")
    }
  }

  it should "concatenate repeated matches without adding separators" in {
    run(JsonPathConfig(Seq("v"))) { serial =>
      serial.send("{\"v\":1,\"v\":\"two\",\"v\":3}\n")
      serial.expect(":1:\"two\":3")
    }
  }

  it should "extract during reception and wait for LF before resetting the record" in {
    run(JsonPathConfig(Seq("v"))) { serial =>
      // Exactly eight bytes: the first word can be parsed before LF arrives.
      serial.send("{\"v\": 1,")
      serial.expect(": 1")
      serial.send("\"v\":2}")
      serial.expect("") // Partial word has not been flushed yet.
      serial.send("\n")
      serial.expect(":2")
      // The root closes in a full word; even after TX drains, wait for LF.
      serial.send("{\"v\": 3}")
      serial.expect(": 3")
      serial.send("\n")
      serial.expect("")
      serial.send("{\"v\":4}\n")
      serial.expect(":4")
    }
  }

  it should "preserve bytes across memory word boundaries" in {
    run(JsonPathConfig(Seq("text"))) { serial =>
      val value = "0123456789ab"
      serial.send(s"{\"text\": \"$value\"}\n")
      serial.expect(s": \"$value\"")
    }
  }
}
