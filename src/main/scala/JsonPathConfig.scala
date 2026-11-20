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
}


/** Configuration for the currently supported, object-key-only JSON path. */
final case class JsonPathConfig(
  path: Seq[String],
  maxDepth: Int = 8,
  attributeId: Int = 0,
  additionalPaths: Seq[Seq[String]] = Seq.empty,
  valueType: ValueType = ValueType.Raw
) {
  require(path.nonEmpty, "path must contain at least one object key")
  require(allPaths.forall(_.nonEmpty), "paths must contain at least one object key")
  require(allPaths.flatten.forall(_.nonEmpty), "path keys must not be empty")
  require(maxDepth >= 1, "maxDepth must be at least one")
  require(attributeId >= 0 && attributeId <= 255, "attributeId must fit in 8 bits")

  /** Stable key-symbol codes expected from the path encoder. */
  def allPaths: Seq[Seq[String]] = path +: additionalPaths

  def keyIds: Map[String, Int] = allPaths.flatten.distinct.zipWithIndex.toMap
}
