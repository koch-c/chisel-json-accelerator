import org.scalatest.flatspec.AnyFlatSpec

class JsonPathConfigTest extends AnyFlatSpec {
  "JsonPathConfig" should "derive field and key IDs independently" in {
    val fields = Seq(
      FieldSpec(Seq("user", "id"), ValueType.Int64),
      FieldSpec(Seq("id"), ValueType.Int32)
    )
    val config = JsonPathConfig(fields)
    assert(config.fields == fields)
    assert(config.attributeIds == Map(Seq("user", "id") -> 0, Seq("id") -> 1))
    assert(config.keyIds == Map("user" -> 0, "id" -> 1))
  }

  it should "reject invalid or ambiguous paths" in {
    val field = FieldSpec(Seq("id"))
    intercept[IllegalArgumentException](JsonPathConfig(Nil))
    intercept[IllegalArgumentException](JsonPathConfig(Seq(FieldSpec(Nil))))
    intercept[IllegalArgumentException](JsonPathConfig(Seq(FieldSpec(Seq("")))))
    intercept[IllegalArgumentException](JsonPathConfig(Seq(field, field)))
    intercept[IllegalArgumentException](
      JsonPathConfig(Seq(field, FieldSpec(Seq("id", "nested"))))
    )
    intercept[IllegalArgumentException](
      JsonPathConfig(Seq(FieldSpec(Seq("user", "id"))), maxDepth = 1)
    )
  }

  it should "limit field IDs to eight bits" in {
    val fields = (0 until 257).map(i => FieldSpec(Seq(s"key$i")))
    assert(JsonPathConfig(fields.take(256)).attributeIds(Seq("key255")) == 255)
    intercept[IllegalArgumentException](JsonPathConfig(fields))
  }
}
