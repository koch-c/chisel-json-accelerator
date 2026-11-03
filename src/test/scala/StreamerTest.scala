import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class StreamerSpec extends AnyFlatSpec with ChiselScalatestTester {
  def initialize(c: Streamer): Unit = {
    c.io.in.valid.poke(false.B)
    c.io.in.bits.poke(0.U)
    c.io.out.ready.poke(false.B)
  }

  def waitFor(c: Streamer)(condition: => Boolean): Unit = {
    var cycles = 0
    while (!condition && cycles < 100) {
      c.clock.step()
      cycles += 1
    }
    assert(condition, "Timed out waiting for a Streamer handshake")
  }

  // Keep each byte stable until accepted, then move to the next byte.
  def send(c: Streamer, bytes: String): Unit = {
    for (char <- bytes) {
      c.io.in.valid.poke(true.B)
      c.io.in.bits.poke(char.toInt.U)
      waitFor(c)(c.io.in.ready.peek().litToBoolean)
      c.clock.step()
    }
    c.io.in.valid.poke(false.B)
  }

  def validateWord(c: Streamer, word: String): Unit = {
    require(word.length == 8)
    for ((char, i) <- word.zipWithIndex) c.io.out.bits(i).expect(char.toInt.U)
  }

  def consume(c: Streamer, word: String): Unit = {
    c.io.out.ready.poke(true.B)
    waitFor(c)(c.io.out.valid.peek().litToBoolean)
    validateWord(c, word)
    c.clock.step()
    c.io.out.ready.poke(false.B)
  }

  def expectEmpty(c: Streamer): Unit = {
    for (_ <- 0 until 12) {
      c.io.out.valid.expect(false.B)
      c.clock.step()
    }
  }

  "Streamer" should "hold a queued word stable while accepting more input" in {
    test(new Streamer).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      initialize(c)
      send(c, "abcdefgh")
      waitFor(c)(c.io.out.valid.peek().litToBoolean)
      c.io.in.ready.expect(true.B)
      send(c, "ijklmnop")
      for (_ <- 0 until 3) {
        c.io.out.valid.expect(true.B)
        validateWord(c, "abcdefgh")
        c.clock.step()
      }
      consume(c, "abcdefgh")
      consume(c, "ijklmnop")
      expectEmpty(c)
    }
  }

  for (json <- Seq("{\"a\":1}", "{}")) {
    it should s"pad newline-terminated JSON $json and resume collecting" in {
      test(new Streamer).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        initialize(c)
        send(c, json)
        c.clock.step(3)
        c.io.out.valid.expect(false.B) // Silence must not flush partial data.
        send(c, "\n")
        waitFor(c)(c.io.out.valid.peek().litToBoolean)
        c.io.in.ready.expect(true.B)
        for (_ <- 0 until 3) {
          validateWord(c, json.padTo(8, ' '))
          c.io.out.valid.expect(true.B)
          c.clock.step()
        }
        // Collection can resume before the padded word is consumed.
        send(c, "12345678\n")
        consume(c, json.padTo(8, ' '))
        consume(c, "12345678")
        expectEmpty(c) // An aligned document's newline adds no extra word.
      }
    }
  }

  it should "buffer the temperature and humidity JSON and drain it in order" in {
    test(new Streamer).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      initialize(c)
      val json = "{ \"temperature\": 25, \"humidity\": 60 }"
      send(c, json + "\n")
      for (word <- json.grouped(8)) consume(c, word.padTo(8, ' '))
      expectEmpty(c)
    }
  }

  it should "stream a large newline-terminated JSON through repeated output stalls" in {
    test(new Streamer).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      initialize(c)
      c.clock.setTimeout(20000)
      val records = (0 until 64).map { i =>
        s"""{"id":$i,"temperature":${20 + i},"label":"sensor-$i"}"""
      }
      val document = records.mkString("{\"sensors\":[", ",", "]}")
      // Keep valid JSON whitespace while ensuring the final word needs padding.
      val json = if (document.length % 8 == 0) document + " " else document
      assert(json.length > 1024 && json.length % 8 != 0)
      val input = json + "\n"
      val expected = json.grouped(8).map(_.padTo(8, ' ')).toVector
      var sent = 0
      var received = 0
      var cycles = 0
      var blockedCycles = 0
      var heldWord: Option[Seq[BigInt]] = None
      val budget = input.length * 5

      while ((sent < input.length || received < expected.length) && cycles < budget) {
        val inputValid = sent < input.length
        c.io.in.valid.poke(inputValid.B)
        c.io.in.bits.poke((if (inputValid) input(sent).toInt else 0).U)
        // Long stalls fill the queue; ready periods then drain it repeatedly.
        val outputReady = cycles % 200 >= 120
        c.io.out.ready.poke(outputReady.B)
        val inputReady = c.io.in.ready.peek().litToBoolean
        val outputValid = c.io.out.valid.peek().litToBoolean

        heldWord.foreach { word =>
          c.io.out.valid.expect(true.B)
          for ((byte, index) <- word.zipWithIndex) c.io.out.bits(index).expect(byte.U)
        }
        if (outputValid) {
          assert(received < expected.length, "Streamer emitted an extra word")
          validateWord(c, expected(received))
        }
        heldWord = if (outputValid && !outputReady)
          Some((0 until 8).map(i => c.io.out.bits(i).peek().litValue))
        else None

        if (inputValid && inputReady) sent += 1
        if (inputValid && !inputReady) blockedCycles += 1
        if (outputValid && outputReady) received += 1
        c.clock.step()
        cycles += 1
      }

      assert(sent == input.length, "Timed out sending the document and newline")
      assert(received == expected.length, "Timed out receiving all JSON words")
      assert(blockedCycles > 0, "The transfer did not exercise input backpressure")
      c.io.in.valid.poke(false.B)
      c.io.out.ready.poke(true.B)
      expectEmpty(c)
    }
  }

  it should "backpressure input when all buffers fill and recover without losing bytes" in {
    test(new Streamer).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      initialize(c)
      // Eight queued words plus one completed word in the collection buffer.
      val words = (0 until 9).map(i => s"word000$i")
      send(c, words.mkString)
      c.io.in.valid.poke(true.B)
      c.io.in.bits.poke('Z'.toInt.U)
      for (_ <- 0 until 4) {
        c.io.in.ready.expect(false.B)
        c.io.out.valid.expect(true.B)
        validateWord(c, words.head)
        c.clock.step()
      }
      consume(c, words.head)
      // Keep Z offered until space is available and it is accepted exactly once.
      waitFor(c)(c.io.in.ready.peek().litToBoolean)
      c.clock.step()
      c.io.in.valid.poke(false.B)
      send(c, "1234567")
      for (word <- words.tail) consume(c, word)
      consume(c, "Z1234567")
      expectEmpty(c)
    }
  }
}
