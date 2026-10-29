import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ParserSpec extends AnyFlatSpec with ChiselScalatestTester {

  val exampleJson = """{ "temperature": 25, "humidity": 60 }"""

  // Send one 8-char word: wait until the parser is ready, then hand it over
  def sendWord(c: Parser, word: String): Unit = {
    c.io.value.ready.poke(true.B) // always take value bytes
    for ((char, i) <- word.zipWithIndex) c.io.in.bits(i).poke(char.toInt.U)
    c.io.in.valid.poke(true.B)
    while (!c.io.in.ready.peek().litToBoolean) c.clock.step()
    c.clock.step() // the word is taken on this clock edge
    c.io.in.valid.poke(false.B)
  }

  // Wait until the parser has processed all 8 chars of the current word
  def waitUntilIdle(c: Parser): Unit = {
    while (!c.io.in.ready.peek().litToBoolean) c.clock.step()
  }

  // Split a string into 8-char words (padded with spaces) and send them all
  def sendJson(c: Parser, json: String): Unit = {
    val padded = json.padTo((json.length + 7) / 8 * 8, ' ')
    for (word <- padded.grouped(8)) sendWord(c, word)
    waitUntilIdle(c)
  }

  // Send a JSON cycle by cycle and collect the value bytes until done.
  // With slowReader the value output is only ready every 4th cycle.
  // ADHOC reader helper function, to test value return functionality
  def readValue(c: Parser, json: String, slowReader: Boolean = false): String = {
    val words = json.padTo((json.length + 7) / 8 * 8, ' ').grouped(8).toSeq
    val value = new StringBuilder
    var sent = 0
    var cycle = 0
    while (!c.io.done.peek().litToBoolean && cycle < 5000) {
      if (sent < words.length) {
        for ((char, i) <- words(sent).zipWithIndex) c.io.in.bits(i).poke(char.toInt.U)
      }
      c.io.in.valid.poke((sent < words.length).B)
      c.io.value.ready.poke((!slowReader || cycle % 4 == 0).B)
      if (sent < words.length && c.io.in.ready.peek().litToBoolean) sent += 1
      if (c.io.value.valid.peek().litToBoolean && c.io.value.ready.peek().litToBoolean) {
        value += c.io.value.bits.peek().litValue.toChar
      }
      c.clock.step()
      cycle += 1
    }
    value.toString
  }

  "Parser" should "find temperature in the example JSON" in {
    test(new Parser("temperature")) { c =>
      sendJson(c, exampleJson)
      c.io.found.expect(true.B)
    }
  }

  it should "only raise found after the colon, and done after the value" in {
    test(new Parser("temperature")) { c =>
      // Word 1: { "tempe  -> key only partially seen
      sendWord(c, """{ "tempe""")
      waitUntilIdle(c)
      c.io.found.expect(false.B)
      c.io.done.expect(false.B)

      // Word 2: rature":  -> key complete, followed by ':'
      sendWord(c, """rature":""")
      waitUntilIdle(c)
      c.io.found.expect(true.B)
      c.io.done.expect(false.B)

      // Word 3: the value ends at '}'
      sendWord(c, """ 25 }   """)
      waitUntilIdle(c)
      c.io.done.expect(true.B)
    }
  }

  it should "report not found when temperature is missing" in {
    test(new Parser("temperature")) { c =>
      sendJson(c, """{ "humidity": 60 }""")
      c.io.found.expect(false.B)
    }
  }

  it should "not match temperature when it is a value" in {
    test(new Parser("temperature")) { c =>
      sendJson(c, """{ "sensor": "temperature" }""")
      c.io.found.expect(false.B)
    }
  }

  it should "keep searching after a nested object closes" in {
    test(new Parser("temperature")) { c =>
      sendJson(c, """{ "meta": { "id": 1 }, "temperature": 25 }""")
      c.io.found.expect(true.B)
    }
  }

  it should "find a key inside a nested object" in {
    test(new Parser("temperature")) { c =>
      sendJson(c, """{ "sensor": { "temperature": 25 } }""")
      c.io.found.expect(true.B)
    }
  }

  it should "find a key inside an array of objects" in {
    test(new Parser("temperature")) { c =>
      sendJson(c, """[ { "humidity": 60 }, { "temperature": 25 } ]""")
      c.io.found.expect(true.B)
    }
  }

  it should "not end a string on an escaped quote" in {
    test(new Parser("temperature")) { c =>
      sendJson(c, """{ "note": "say \"temperature\": hi", "humidity": 60 }""")
      c.io.found.expect(false.B)
      c.io.done.expect(true.B)
    }
  }

  it should "handle an escaped backslash at the end of a string" in {
    test(new Parser("temperature")) { c =>
      sendJson(c, """{ "path": "C:\\", "temperature": 25 }""")
      c.io.found.expect(true.B)
    }
  }

  it should "find a key within a nested JSON" in {
    val deepJson =
      """{
        |  "nebula": {
        |    "x7q_pulse": {
        |      "crystal_node": {
        |        "You found me": "Hidden value discovered!",
        |        "flux_rate": 73,
        |        "active": true
        |      },
        |      "orbit": "silver"
        |    },
        |    "drift": 42
        |  },
        |  "vortex": {
        |    "kappa_91": {
        |      "signal": {
        |        "frequency": 8.62,
        |        "status": "stable"
        |      }
        |    }
        |  }
        |}""".stripMargin
    test(new Parser("You found me")) { c =>
      sendJson(c, deepJson)
      c.io.found.expect(true.B)
      c.io.done.expect(true.B)
    }
  }

  val escapedJson =
    """{
      |  "nebula\ncluster": {
      |    "x7q\tpulse": {
      |      "crystal\"node": {
      |        "You found me": "Hidden value discovered!",
      |        "line\\break": "Found\\Hidden",
      |        "quote\"key": "escaped quote",
      |        "forward\/slash": "slash value"
      |      },
      |      "orbit\rpath": "silver"
      |    },
      |    "drift\bzone": 42
      |  },
      |  "vortex\fcore": {
      |    "kappa_91": {
      |      "signal\ntrace": {
      |        "frequency": 8.62,
      |        "status": "stable"
      |      }
      |    }
      |  }
      |}""".stripMargin //just to make the JSON human readable

  it should "stay in sync through every kind of escape sequence" in {
    test(new Parser("status")) { c => // last key, after all the escapes
      sendJson(c, escapedJson)
      c.io.found.expect(true.B)
      c.io.done.expect(true.B)
    }
  }

  it should "find a key containing an escaped quote" in {
    test(new Parser("""quote\"key""")) { c =>
      sendJson(c, escapedJson)
      c.io.found.expect(true.B)
    }
  }

  it should "find a key containing an escaped backslash" in {
    test(new Parser("""line\\break""")) { c =>
      sendJson(c, escapedJson)
      c.io.found.expect(true.B)
    }
  }

  it should "only be done when the outermost object closes" in {
    test(new Parser("temperature")) { c =>
      sendJson(c, """{ "a": { "b": [1, 2] }, "c": 3 }""")
      c.io.found.expect(false.B)
      c.io.done.expect(true.B)
    }
  }

  it should "return a number value" in {
    test(new Parser("temperature")) { c =>
      assert(readValue(c, exampleJson) == "25")
      c.io.found.expect(true.B)
    }
  }

  it should "return a string value with its quotes" in {
    test(new Parser("You found me")) { c =>
      assert(readValue(c, """{ "You found me": "Hidden value discovered!" }""") == "\"Hidden value discovered!\"")
    }
  }

  it should "not end a string value at a comma, brace or escaped quote" in {
    test(new Parser("note")) { c =>
      val value = readValue(c, """{ "note": "a, b} \"c\" d", "x": 1 }""")
      assert(value == """"a, b} \"c\" d"""")
    }
  }
  
  it should "return nothing when the key is missing" in {
    test(new Parser("temperature")) { c =>
      assert(readValue(c, """{ "humidity": 60 }""") == "")
      c.io.found.expect(false.B)
    }
  }

  it should "return an object value including its nested braces" in {
    test(new Parser("location")) { c =>
      val value = readValue(c, """{ "location": { "lat": 55.7, "pos": { "x": 1 } }, "id": 3 }""")
      assert(value == """{"lat":55.7,"pos":{"x":1}}""")
    }
  }

  it should "return an object value that is the last member" in {
    test(new Parser("location")) { c =>
      assert(readValue(c, """{ "id": 3, "location": { "lat": 55.7 } }""") == """{"lat":55.7}""")
    }
  }

  it should "return an array value" in {
    test(new Parser("readings")) { c =>
      assert(readValue(c, """{ "readings": [ 1, [2, 3], { "a": 4 } ], "id": 3 }""") == """[1,[2,3],{"a":4}]""")
    }
  }

  it should "not restart the value when it contains the same key" in {
    test(new Parser("a")) { c =>
      assert(readValue(c, """{ "a": { "a": 1 }, "b": 2 }""") == """{"a":1}""")
    }
  }
}
