import org.scalatest.flatspec.AnyFlatSpec

class OutputLayoutSpec extends AnyFlatSpec {
  "OutputLayout" should "place attributes at running offsets in JSONPath order" in {
    val layout = OutputLayout(Seq(IntAttr(64), StringAttr(32), IntAttr(32), FloatAttr(32)))
    assert(layout.offsets == Seq(0, 8, 40, 44))
    assert(layout.recordBytes == 48)
    assert(layout.presentBytes == 1)
    assert(layout.frameBytes == 49)
  }

  it should "use one presence bit per attribute, rounded up to whole bytes" in {
    assert(OutputLayout(Seq.fill(8)(IntAttr(32))).presentBytes == 1)
    val nine = OutputLayout(Seq.fill(9)(IntAttr(32)))
    assert(nine.presentBytes == 2)
    assert(nine.frameBytes == 2 + 36)
  }

  it should "reject unsupported attribute sizes and an empty layout" in {
    intercept[IllegalArgumentException] { IntAttr(16) }
    intercept[IllegalArgumentException] { FloatAttr(16) }
    intercept[IllegalArgumentException] { StringAttr(0) }
    intercept[IllegalArgumentException] { OutputLayout(Nil) }
  }

  it should "accept filters whose constant matches the attribute type" in {
    OutputLayout(
      Seq(IntAttr(32), FloatAttr(32), StringAttr(4)),
      Seq(
        Comparison(0, CompareOp.Ge, IntLit(-5)),
        Comparison(1, CompareOp.Le, FloatLit(38.55)),
        Comparison(1, CompareOp.Ge, IntLit(6)),
        Comparison(2, CompareOp.Eq, StrLit("john"))
      )
    )
  }

  it should "reject filters that cannot be built" in {
    val attributes = Seq(IntAttr(32), FloatAttr(32), StringAttr(4))
    def filter(comparison: Comparison): Unit = {
      intercept[IllegalArgumentException] { OutputLayout(attributes, Seq(comparison)) }
    }
    filter(Comparison(3, CompareOp.Eq, IntLit(1))) // No such attribute.
    filter(Comparison(0, CompareOp.Eq, IntLit(BigInt(1) << 31))) // Does not fit int(32).
    filter(Comparison(0, CompareOp.Eq, StrLit("1")))
    filter(Comparison(0, CompareOp.Eq, FloatLit(1.0)))
    filter(Comparison(1, CompareOp.Eq, FloatLit(Double.NaN)))
    filter(Comparison(2, CompareOp.Eq, IntLit(1)))
    filter(Comparison(2, CompareOp.Lt, StrLit("john"))) // Strings only support =.
    filter(Comparison(2, CompareOp.Eq, StrLit("johnny"))) // Longer than string(4).
  }
}
