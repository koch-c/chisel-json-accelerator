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


  "Streamer" should "send 5 words" in {
    test(new Streamer("""{ "temperature": 25, "humidity": 60 }""")) { c =>
        validateWord(c, """{ "tempe""")

        waitUntilIdle(c)
        validateWord(c, """rature":""")

        waitUntilIdle(c)
        validateWord(c, """ 25, "hu""")

        waitUntilIdle(c)
        validateWord(c, """midity":""")
        
        waitUntilIdle(c)
        validateWord(c, """ 60 }   """)

        c.clock.step(1)
        c.io.out.valid.expect(false.B)


    }
  }

}
