import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ParserSpec extends AnyFlatSpec with ChiselScalatestTester {

  val exampleJson = """{ "temperature": 25, "humidity": 60 }"""

  // Send one 8-char word: wait until the parser is ready, then hand it over
  def sendWord(c: Parser, word: String): Unit = {
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

  "Parser" should "find temperature in the example JSON" in {
    test(new Parser("temperature")) { c =>
      sendJson(c, exampleJson)
      c.io.found.expect(true.B)
      c.io.notFound.expect(false.B)
    }
  }

  it should "only raise found after the colon following the key" in {
    test(new Parser("temperature")) { c =>
      // Word 1: { "tempe  -> key only partially seen
      sendWord(c, """{ "tempe""")
      waitUntilIdle(c)
      c.io.found.expect(false.B)

      // Word 2: rature":  -> key complete, followed by ':'
      sendWord(c, """rature":""")
      waitUntilIdle(c)
      c.io.found.expect(true.B)
    }
  }

  it should "report not found when temperature is missing" in {
    test(new Parser("temperature")) { c =>
      sendJson(c, """{ "humidity": 60 }""")
      c.io.found.expect(false.B)
      c.io.notFound.expect(true.B)
    }
  }

  it should "not match temperature when it is a value" in {
    test(new Parser("temperature")) { c =>
      sendJson(c, """{ "sensor": "temperature" }""")
      c.io.found.expect(false.B)
      c.io.notFound.expect(true.B)
    }
  }
}
