import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/** Parts 3, 4a, 4b and 5 together, with the StringParser from string-parser. */
class StringOutputBridgeSpec extends AnyFlatSpec with ChiselScalatestTester {
  import ExecutionHarnessDriver._

  private def harness(layout: OutputLayout) = new ExecutionHarness(
    layout,
    Some { () =>
      val parser = Module(new StringParser)
      (parser.io.in, parser.io.out, parser.io.last)
    }
  )

  "The execution stage" should "deliver StringParser strings to the OutputStage" in {
    val layout = OutputLayout(
      Seq(
        IntAttr(32),
        StringAttr(8),
        StringAttr(4),
        StringAttr(8),
        StringAttr(3)
      )
    )
    val json =
      """{"id":7,"name":"john","bad":12,"esc":"a\"b\n","long":"abcdef"}"""
    test(harness(layout)) { c =>
      val (output, _) = playRecord(
        c,
        json,
        Seq(0 -> "id", 1 -> "name", 2 -> "bad", 3 -> "esc", 4 -> "long")
      )
      val expected =
        Seq(0x1b) ++ le(7, 4) ++ text("john", 8) ++ Seq.fill(4)(0) ++
          Seq('a', '"', 'b', '\n').map(_.toInt).padTo(8, 0) ++ text("abc", 3)
      assert(output == expected, s"Unexpected output: $output")
    }
  }

  it should "filter on a decoded string" in {
    val layout = OutputLayout(
      Seq(IntAttr(32), StringAttr(8)),
      Seq(Comparison(1, CompareOp.Eq, StrLit("jo\"n")))
    )
    test(harness(layout)) { c =>
      val (kept, _) =
        playRecord(
          c,
          """{"id":1,"name":"jo\"n"}""",
          Seq(0 -> "id", 1 -> "name")
        )
      assert(kept == Seq(0x03) ++ le(1, 4) ++ text("jo\"n", 8))
      val (gone, dropped) =
        playRecord(c, """{"name":"john","id":2}""", Seq(1 -> "name", 0 -> "id"))
      assert(gone.isEmpty && dropped == 1)
    }
  }
}
