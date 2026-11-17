import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class LexerSpec extends AnyFlatSpec with ChiselScalatestTester {
  // keyId is only meaningful for Key symbols, so it is zeroed for the others.
  private case class Sym(kind: BigInt, keyId: BigInt, addr: BigInt)
  private case class Run(symbols: Seq[Sym], batches: Seq[Int], stalls: Int, error: Boolean)

  private val keyKind = SymbolKind.Key.litValue

  private def read(s: JsonSymbol): Sym = {
    val kind = s.kind.peek().litValue
    Sym(kind, if (kind == keyKind) s.keyId.peek().litValue else 0, s.addr.peek().litValue)
  }

  /** "{@0 K1@9 :@10" style spec; K<n> is key n. */
  private def symbols(spec: String): Seq[Sym] = spec.split(" ").toSeq.map { token =>
    val Array(k, a) = token.split("@")
    val kind = k match {
      case "{" => SymbolKind.ObjOpen
      case "}" => SymbolKind.ObjClose
      case "[" => SymbolKind.ArrOpen
      case "]" => SymbolKind.ArrClose
      case ":" => SymbolKind.Colon
      case "," => SymbolKind.Comma
      case _   => SymbolKind.Key
    }
    Sym(kind.litValue, if (k.startsWith("K")) BigInt(k.tail) else 0, a.toInt)
  }

  /** Kinds only, e.g. "{K0:,}". */
  private def kinds(run: Run): String = run.symbols.map { s =>
    if (s.kind == keyKind) s"K${s.keyId}"
    else Map(
      SymbolKind.ObjOpen.litValue -> "{", SymbolKind.ObjClose.litValue -> "}",
      SymbolKind.ArrOpen.litValue -> "[", SymbolKind.ArrClose.litValue -> "]",
      SymbolKind.Colon.litValue -> ":", SymbolKind.Comma.litValue -> ","
    )(s.kind)
  }.mkString

  /** Words as the InputFIFO delivers them: lane 0 first, padded with spaces. */
  private def words(text: String): Seq[Seq[Int]] =
    text.getBytes("UTF-8").map(_ & 0xff).toSeq.grouped(8).map(_.padTo(8, ' '.toInt)).toSeq

  private def restart(c: Module): Unit = {
    c.reset.poke(true.B)
    c.clock.step()
    c.reset.poke(false.B)
  }

  /** Sends one record through the parallel lexer, checking that a stalled
    * batch stays stable.
    */
  private def runParallel(c: Lexer, text: String, ready: () => Boolean = () => true): Run = {
    restart(c)
    val ws = words(text)
    val out = ArrayBuffer.empty[Sym]
    val batches = ArrayBuffer.empty[Int]
    var held: Option[Seq[Sym]] = None
    var sent, cycles, idle, stalls = 0
    while (sent < ws.length || idle < 8) { // drain the 4-stage pipeline
      assert(cycles < 20 * ws.length + 100, "Lexer stopped making progress")
      c.io.in.valid.poke((sent < ws.length).B)
      if (sent < ws.length) ws(sent).zipWithIndex.foreach { case (b, l) => c.io.in.bits(l).poke(b.U) }
      val r = ready()
      c.io.out.ready.poke(r.B)
      if (c.io.out.valid.peek().litToBoolean) {
        val n = c.io.out.bits.count.peek().litValue.toInt
        assert(n >= 1 && n <= 8, s"Invalid batch count $n")
        val batch = (0 until n).map(l => read(c.io.out.bits.entries(l)))
        held.foreach(h => assert(h == batch, "Batch changed while stalled"))
        held = if (r) None else Some(batch)
        if (r) { out ++= batch; batches += n }
        idle = 0
      } else {
        assert(held.isEmpty, "Valid dropped while stalled")
        if (sent == ws.length) idle += 1
      }
      val inReady = c.io.in.ready.peek().litToBoolean
      if (sent < ws.length && !inReady) stalls += 1
      c.clock.step()
      cycles += 1
      if (sent < ws.length && inReady) sent += 1
    }
    Run(out.toSeq, batches.toSeq, stalls, c.io.error.peek().litToBoolean)
  }

  /** Same record through the byte-serial reference lexer. */
  private def runSerial(c: SerialLexer, text: String): Run = {
    restart(c)
    val ws = words(text)
    val out = ArrayBuffer.empty[Sym]
    var sent, cycles, idle = 0
    c.io.out.ready.poke(true.B)
    while (sent < ws.length || idle < 12) {
      assert(cycles < 20 * ws.length + 100, "Serial lexer stopped making progress")
      c.io.in.valid.poke((sent < ws.length).B)
      if (sent < ws.length) ws(sent).zipWithIndex.foreach { case (b, l) => c.io.in.bits(l).poke(b.U) }
      if (c.io.out.valid.peek().litToBoolean) { out += read(c.io.out.bits); idle = 0 }
      else if (sent == ws.length) idle += 1
      val inReady = c.io.in.ready.peek().litToBoolean
      c.clock.step()
      cycles += 1
      if (sent < ws.length && inReady) sent += 1
    }
    Run(out.toSeq, Nil, 0, c.io.error.peek().litToBoolean)
  }

  "Lexer" should "emit a packed batch per word with byte addresses" in {
    test(new Lexer(Seq("a", "b"), maxDepth = 8, addrWidth = 12)) { c =>
      // The string "b" opens in word 0 and closes in word 1.
      val run = runParallel(c, """{"a":1,"b":[2,3]}""" + "\n")
      assert(run.symbols == symbols("{@0 K0@3 :@4 ,@6 K1@9 :@10 [@11 ,@13 ]@15 }@16"))
      assert(run.batches == Seq(4, 5, 1))
      assert(run.stalls == 0, "Lexer should accept one word per cycle")
      assert(!run.error)
    }
  }

  it should "fill all eight lanes and skip words without symbols" in {
    test(new Lexer(Seq("a"), maxDepth = 8, addrWidth = 12)) { c =>
      val nested = runParallel(c, "[[[[[[[[]]]]]]]]\n")
      assert(kinds(nested) == "[[[[[[[[]]]]]]]]")
      assert(nested.batches == Seq(8, 8))
      assert(!nested.error)

      val quiet = runParallel(c, """{"a":"0123456789abcdef"}""" + "\n")
      assert(kinds(quiet) == "{K0:}")
      assert(quiet.batches == Seq(3, 1))
    }
  }

  it should "only emit keys in key position, including after nested containers" in {
    test(new Lexer(Seq("temperature"), maxDepth = 8, addrWidth = 12)) { c =>
      val run = runParallel(c,
        """{"x":["temperature",{"temperature":1}],"temperature":"temperature"}""" + "\n")
      assert(kinds(run) == "{:[,{K0:}],K0:}")
      assert(!run.error)
    }
  }

  it should "tell apart keys that are prefixes of each other" in {
    test(new Lexer(Seq("a", "ab"), maxDepth = 8, addrWidth = 12)) { c =>
      val run = runParallel(c, """{"ab":1,"a":2,"abc":3,"":4,"b":5}""" + "\n")
      assert(kinds(run) == "{K1:,K0:,:,:,:}")
    }
  }

  it should "handle escapes across word boundaries" in {
    test(new Lexer(Seq("a"), maxDepth = 8, addrWidth = 12)) { c =>
      val expected = symbols("{@0 :@4 ,@10 K0@13 :@14 }@16")
      // Lane 7 holds a backslash. First it escapes the quote in lane 0 of the
      // next word, then it escapes a second backslash so the quote closes.
      assert(runParallel(c, """{"k":"x\"","a":1}""" + "\n").symbols == expected)
      assert(runParallel(c, """{"k":"x\\","a":1}""" + "\n").symbols == expected)
    }
  }

  it should "report unbalanced and too deeply nested input" in {
    test(new Lexer(Seq("a"), maxDepth = 2, addrWidth = 12)) { c =>
      assert(!runParallel(c, "[[]]\n").error)
      assert(runParallel(c, "[[[]]]\n").error, "depth overflow")
      assert(runParallel(c, "{}}\n").error, "close at depth zero")
      assert(runParallel(c, "{]\n").error, "mismatched close")
      assert(runParallel(c, "[{]}\n").error, "mismatched close inside a word")
    }
  }

  private def randomRecord(r: Random, keys: Seq[String]): String = {
    def pick[T](xs: Seq[T]): T = xs(r.nextInt(xs.length))
    def ws(): String = if (r.nextInt(3) == 0) pick(Seq(" ", "  ", "\t", "\r", "      ")) else ""
    def str(): String = Seq.fill(r.nextInt(7))(r.nextInt(10) match {
      case 0 => "\\\"" // \"
      case 1 => "\\\\" // \\ , often right before the closing quote
      case 2 => "\\n"
      case 3 => "\\u0041"
      case 4 => pick(Seq("{", "}", "[", "]", ":", ","))
      case _ => r.alphanumeric.take(1 + r.nextInt(4)).mkString
    }).mkString("\"", "", "\"")
    def key(): String = r.nextInt(5) match {
      case 0 | 1 => "\"" + pick(keys) + "\""
      case 2     => val k = pick(keys); "\"" + (if (r.nextBoolean()) k.init else k + "z") + "\""
      case _     => str()
    }
    def value(depth: Int): String = r.nextInt(if (depth >= 5) 4 else 7) match {
      case 0 => pick(Seq("0", "-12", "3.25", "1e+9", "-0.5E-3"))
      case 1 => pick(Seq("true", "false", "null"))
      case 2 => "\"" + pick(keys) + "\"" // a value that looks like a key
      case 3 => str()
      case 4 | 5 => obj(depth + 1)
      case _ => arr(depth + 1)
    }
    def obj(depth: Int): String =
      Seq.fill(r.nextInt(5))(ws() + key() + ws() + ":" + ws() + value(depth) + ws())
        .mkString("{", ",", "}")
    def arr(depth: Int): String =
      Seq.fill(r.nextInt(5))(ws() + value(depth) + ws()).mkString("[", ",", "]")
    ws() + obj(1) + ws() + "\n"
  }

  it should "match the serial lexer on random JSON under backpressure" in {
    val keys = Seq("a", "ab", "temperature", "x")
    val rng = new Random(2026)
    val records = Seq.fill(60)(randomRecord(rng, keys))

    var expected = Seq.empty[Run]
    test(new SerialLexer(keys, maxDepth = 8, addrWidth = 12)) { c =>
      expected = records.map(runSerial(c, _))
    }
    assert(expected.forall(!_.error), "Generator produced invalid JSON")
    assert(expected.map(_.symbols.count(_.kind == keyKind)).sum > 50, "Too few keys exercised")

    test(new Lexer(keys, maxDepth = 8, addrWidth = 12)) { c =>
      val stall = new Random(7)
      for ((record, want) <- records.zip(expected)) {
        val got = runParallel(c, record, () => stall.nextInt(3) != 0)
        assert(got.symbols == want.symbols, s"Symbol stream differs for record: $record")
        assert(!got.error, s"Unexpected error for record: $record")
      }
    }
  }
}
