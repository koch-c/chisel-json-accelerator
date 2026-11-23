import org.scalatest.flatspec.AnyFlatSpec

class JsonPathConfigSpec extends AnyFlatSpec {
  private val twoPaths = Seq(Seq("id"), Seq("user", "name"))

  "JsonPathConfig" should "use valueType for every path by default" in {
    val config = JsonPathConfig(twoPaths, valueType = ValueType.Number)
    assert(config.valueTypeOf(0) == ValueType.Number)
    assert(config.valueTypeOf(1) == ValueType.Number)
    assert(config.usedValueTypes == Seq(ValueType.Number))
  }

  it should "let each path have its own value type" in {
    val config = JsonPathConfig(
      twoPaths,
      valueTypes = Seq(ValueType.Int32, ValueType.String)
    )
    assert(config.valueTypeOf(0) == ValueType.Int32)
    assert(config.valueTypeOf(1) == ValueType.String)
    assert(config.usedValueTypes == Seq(ValueType.Int32, ValueType.String))
  }

  it should "list a value type once when several paths share it" in {
    val config = JsonPathConfig(
      Seq(Seq("a"), Seq("b"), Seq("c")),
      valueTypes = Seq(ValueType.String, ValueType.Int32, ValueType.String)
    )
    assert(config.usedValueTypes == Seq(ValueType.String, ValueType.Int32))
  }

  it should "number the attribute ids from attributeId in path order" in {
    val config = JsonPathConfig(twoPaths, attributeId = 5)
    assert(config.attributeIdOf(0) == 5)
    assert(config.attributeIdOf(1) == 6)
  }

  it should "reject illegal configurations" in {
    intercept[IllegalArgumentException] { JsonPathConfig(Seq.empty) }
    intercept[IllegalArgumentException] { JsonPathConfig(Seq(Seq.empty)) }
    intercept[IllegalArgumentException] { JsonPathConfig(Seq(Seq(""))) }
    intercept[IllegalArgumentException] {
      JsonPathConfig(Seq(Seq("a"), Seq("a")))
    }
    intercept[IllegalArgumentException] {
      JsonPathConfig(twoPaths, valueTypes = Seq(ValueType.Int32))
    }
    intercept[IllegalArgumentException] {
      JsonPathConfig(twoPaths, attributeId = 255)
    }
    intercept[IllegalArgumentException] {
      JsonPathConfig(twoPaths, parsersPerType = 0)
    }
    intercept[IllegalArgumentException] {
      JsonPathConfig(twoPaths, maxDepth = 0)
    }
  }

  it should "accept the largest attribute id that still fits" in {
    assert(JsonPathConfig(twoPaths, attributeId = 254).attributeIdOf(1) == 255)
  }
}
