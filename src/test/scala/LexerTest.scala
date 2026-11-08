import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class LexerSpec extends AnyFlatSpec with ChiselScalatestTester {

  val kindNames = Seq("ObjOpen", "ObjClose", "ArrOpen", "ArrClose", "Colon", "Comma", "Key")

  // Turn one output symbol into "kind@addr"
  def symbolName(out: JsonSymbol, keys: Seq[String]): String = {
    val kind = out.kind.peek().litValue
    val name =
      if (kind == SymbolKind.Key.litValue) keys(out.keyId.peek().litValue.toInt)
      else kindNames(kind.toInt)
    s"$name@${out.addr.peek().litValue}"
  }

  // Feed the JSON as space-padded 8-byte words, like the Streamer sends them,
  // and collect the symbols until the last word is lexed.
  // With slowReader the output is only ready every 3rd cycle.
  def lex(c: Lexer, json: String, keys: Seq[String], slowReader: Boolean = false): Seq[String] = {
    val words = json.padTo((json.length + 7) / 8 * 8, ' ').grouped(8).toSeq
    val symbols = scala.collection.mutable.ArrayBuffer[String]()
    var sent = 0
    var cycle = 0
    var done = false
    while (!done && cycle < 10000) {
      if (sent < words.length) {
        for ((char, i) <- words(sent).zipWithIndex) c.io.in.bits(i).poke(char.toInt.U)
      }
      c.io.in.valid.poke((sent < words.length).B)
      c.io.out.ready.poke((!slowReader || cycle % 3 == 0).B)
      if (c.io.out.valid.peek().litToBoolean && c.io.out.ready.peek().litToBoolean) {
        symbols += symbolName(c.io.out.bits, keys)
      }
      val ready = c.io.in.ready.peek().litToBoolean
      if (ready && sent < words.length) sent += 1
      else if (ready) done = true // last byte of the last word leaves on this edge
      c.clock.step()
      cycle += 1
    }
    symbols.toSeq
  }

  // Paper Figure 1 style record, path $.(id | user.name)
  val keys = Seq("id", "user", "name")
  val record = """{"id":1,"user":{"id":2,"name":"x"}}"""
  //              0123456789012345678901234567890123
  val expected = Seq(
    "ObjOpen@0", "id@4", "Colon@5", "Comma@7",
    "user@13", "Colon@14", "ObjOpen@15", "id@19", "Colon@20", "Comma@22",
    "name@28", "Colon@29", "ObjClose@33", "ObjClose@34"
  )

  "Lexer" should "turn the paper's example into structural and key symbols" in {
    test(new Lexer(keys)) { c =>
      assert(lex(c, record, keys) == expected)
      c.io.error.expect(false.B)
    }
  }

  it should "give the same symbols when the reader stalls" in {
    test(new Lexer(keys)) { c =>
      assert(lex(c, record, keys, slowReader = true) == expected)
    }
  }

  it should "ignore keys not in the path and strings in value position" in {
    test(new Lexer(keys)) { c =>
      val json = """{"idx":"id","ids":["name"]}"""
      assert(lex(c, json, keys) ==
        Seq("ObjOpen@0", "Colon@6", "Comma@11", "Colon@17", "ArrOpen@18", "ArrClose@25", "ObjClose@26"))
    }
  }

  it should "hide structural characters and escaped quotes inside strings" in {
    test(new Lexer(keys)) { c =>
      val json = """{"a":"{,]:\"id\"","name":0}"""
      assert(lex(c, json, keys) == Seq("ObjOpen@0", "Colon@4", "Comma@17", "name@23", "Colon@24", "ObjClose@26"))
    }
  }

  it should "treat strings after a comma in an array as values" in {
    test(new Lexer(keys)) { c =>
      val json = """[{"id":1},"id"]"""
      assert(lex(c, json, keys) ==
        Seq("ArrOpen@0", "ObjOpen@1", "id@5", "Colon@6", "ObjClose@8", "Comma@9", "ArrClose@14"))
    }
  }

  it should "flag unbalanced brackets" in {
    test(new Lexer(keys)) { c =>
      lex(c, """{"id":1]""", keys)
      c.io.error.expect(true.B)
    }
  }

  it should "flag depth overflow" in {
    test(new Lexer(keys, maxDepth = 2)) { c =>
      lex(c, "[[[]]]", keys)
      c.io.error.expect(true.B)
    }
  }

  it should "wrap the address after the last buffer byte" in {
    test(new Lexer(keys)) { c =>
      c.clock.setTimeout(0) // the spaces give no symbols for 4096 cycles
      // 4096 spaces fill the whole buffer, so the next byte is at address 0 again
      val json = " " * 4096 + """{"id":1}"""
      assert(lex(c, json, keys) == Seq("ObjOpen@0", "id@4", "Colon@5", "ObjClose@7"))
    }
  }

  it should "lex bytes coming through the Streamer" in {
    test(new StreamerLexer(keys)) { c =>
      val bytes = record.padTo((record.length + 7) / 8 * 8, ' ')
      val symbols = scala.collection.mutable.ArrayBuffer[String]()
      var sent = 0
      var cycle = 0
      c.io.out.ready.poke(true.B)
      while (symbols.length < expected.length && cycle < 10000) {
        if (sent < bytes.length) c.io.in.bits.poke(bytes(sent).toInt.U)
        c.io.in.valid.poke((sent < bytes.length).B)
        if (c.io.out.valid.peek().litToBoolean) symbols += symbolName(c.io.out.bits, keys)
        if (sent < bytes.length && c.io.in.ready.peek().litToBoolean) sent += 1
        c.clock.step()
        cycle += 1
      }
      assert(symbols.toSeq == expected)
    }
  }
}

/** UART bytes -> Streamer -> Lexer, as they will sit in the Accelerator. */
class StreamerLexer(keys: Seq[String]) extends Module {
  val io = IO(new Bundle {
    val in  = Flipped(chisel3.util.Decoupled(UInt(8.W)))
    val out = chisel3.util.Decoupled(new JsonSymbol(keys.length))
  })
  val streamer = Module(new Streamer())
  val lexer    = Module(new Lexer(keys))
  streamer.io.in <> io.in
  lexer.io.in <> streamer.io.out
  io.out <> lexer.io.out
}
