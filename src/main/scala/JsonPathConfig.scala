/** How the selected value is delivered on the output. */
sealed trait ValueType
object ValueType {

  /** The raw bytes from the colon up to the next ',' or '}'. */
  case object Raw extends ValueType

  /** Parsed by NumberParser and sent as decimal parts: mantissa and exp10. */
  case object Number extends ValueType

  /** Parsed by NumberParser and sent as a 32-bit two's complement integer. */
  case object Int32 extends ValueType

  /** Parsed by NumberParser and sent as a 64-bit two's complement integer. */
  case object Int64 extends ValueType

  /** Decoded by StringParser and sent without quotes, followed by 0x00 and a
    * flags byte.
    */
  case object String extends ValueType
}

/** Configuration for the currently supported, object-key-only JSON paths. */
final case class JsonPathConfig(
    paths: Seq[Seq[String]],
    maxDepth: Int = 8,
    attributeId: Int = 0,
    valueType: ValueType = ValueType.Raw
) {
  require(paths.nonEmpty, "paths must contain at least one path")
  require(
    paths.forall(_.nonEmpty),
    "paths must contain at least one object key"
  )
  require(paths.flatten.forall(_.nonEmpty), "path keys must not be empty")
  require(maxDepth >= 1, "maxDepth must be at least one")
  require(
    attributeId >= 0 && attributeId <= 255,
    "attributeId must fit in 8 bits"
  )

  /** Distinct keys across all paths, in key-symbol code order. */
  def keys: Seq[String] = paths.flatten.distinct

  /** Stable key-symbol codes expected from the path encoder. */
  def keyIds: Map[String, Int] = keys.zipWithIndex.toMap
}
