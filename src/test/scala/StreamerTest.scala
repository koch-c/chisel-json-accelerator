import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class StreamerSpec extends AnyFlatSpec with ChiselScalatestTester {
  // Simulate the parser has processed all 8 chars of the current word
  def waitUntilIdle(c: Streamer): Unit = {
    c.io.out.ready.poke(false.B)
    c.clock.step(8)
    c.io.out.ready.poke(true.B)
    c.clock.step(1)
  }

  def validateWord(c: Streamer, word: String): Unit = {
    for ((char, i) <- word.zipWithIndex) c.io.out.bits(i).expect(char.toInt.U)
  }

  "Streamer" should "collect eight received bytes into one word" in {
  test(new Streamer).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
    c.io.in.valid.poke(false.B)
    c.io.in.bits.poke(0.U)
    c.io.out.ready.poke(false.B)

    for ((char, index) <- "abcdefgh".zipWithIndex) {
      c.io.in.bits.poke(char.toInt.U)
      c.io.in.valid.poke(true.B)
      c.io.in.ready.expect(true.B)
      c.clock.step() // Accept this byte

      if (index < 7) {
        c.io.out.valid.expect(false.B)
      }
    }

    c.io.in.valid.poke(false.B)

    // A complete word is available, but the consumer is stalled.
    for (_ <- 0 until 3) {
      c.io.out.valid.expect(true.B)
      for ((char, index) <- "abcdefgh".zipWithIndex) {
        c.io.out.bits(index).expect(char.toInt.U)
      }
      c.clock.step()
    }

    // Allow the consumer to take the word.
    c.io.out.ready.poke(true.B)
    c.clock.step()
    c.io.out.valid.expect(false.B)
  }
}
  it should "consume and refill words from the temperature and humidity JSON" in {
    test(new Streamer).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      val json = "{ \"temperature\": 25, \"humidity\": 60 }"
      // The streamer emits complete words, so the sender pads the last group.
      val words = json.grouped(8).map(_.padTo(8, ' ')).toSeq
      c.io.in.valid.poke(false.B)
      c.io.in.bits.poke(0.U)
      c.io.out.ready.poke(false.B)

      for ((word, index) <- words.zipWithIndex) {
        for (char <- word) {
          c.io.out.valid.expect(false.B)
          c.io.in.ready.expect(true.B)
          c.io.in.valid.poke(true.B)
          c.io.in.bits.poke(char.toInt.U)
          c.clock.step()
        }

        // Hold the next group's first byte while the output is stalled.
        c.io.in.valid.poke((index + 1 < words.length).B)
        c.io.in.bits.poke(words.lift(index + 1).map(_.head.toInt).getOrElse(0).U)
        for (_ <- 0 until 3) {
          c.io.in.ready.expect(false.B)
          c.io.out.valid.expect(true.B)
          validateWord(c, word)
          c.clock.step()
        }

        // Consume the word. Input remains blocked on this clock edge.
        c.io.out.ready.poke(true.B)
        c.io.in.ready.expect(false.B)
        c.clock.step()
        c.io.in.valid.poke(false.B)
        c.io.out.ready.poke(false.B)
        c.io.out.valid.expect(false.B)
        c.io.in.ready.expect(true.B)
        c.clock.step(2)
      }
    }
  }
}
