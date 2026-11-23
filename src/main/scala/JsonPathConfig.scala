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

/** Configuration for the currently supported, object-key-only JSON paths.
  *
  * Path i is delivered as valueTypeOf(i) and its frames carry attributeIdOf(i),
  * so the host can tell the paths apart.
  */
final case class JsonPathConfig(
    paths: Seq[Seq[String]],
    maxDepth: Int = 8,
    attributeId: Int = 0,
    valueType: ValueType = ValueType.Raw,
    valueTypes: Seq[ValueType] = Seq.empty,
    parsersPerType: Int = 1
) {
  require(paths.nonEmpty, "paths must contain at least one path")
  require(
    paths.forall(_.nonEmpty),
    "paths must contain at least one object key"
  )
  require(paths.flatten.forall(_.nonEmpty), "path keys must not be empty")
  require(paths.distinct.length == paths.length, "paths must be distinct")
  require(maxDepth >= 1, "maxDepth must be at least one")
  require(
    valueTypes.isEmpty || valueTypes.length == paths.length,
    "valueTypes must be empty or have one entry per path"
  )
  require(
    attributeId >= 0 && attributeId + paths.length - 1 <= 255,
    "the attribute ids of all paths must fit in 8 bits"
  )
  require(parsersPerType >= 1, "parsersPerType must be at least one")

  /** Distinct keys across all paths, in key-symbol code order. */
  def keys: Seq[String] = paths.flatten.distinct

  /** Stable key-symbol codes expected from the path encoder. */
  def keyIds: Map[String, Int] = keys.zipWithIndex.toMap

  /** How the value of path i is delivered. */
  def valueTypeOf(pathIndex: Int): ValueType =
    if (valueTypes.isEmpty) valueType else valueTypes(pathIndex)

  /** The attribute id in the frames of path i. */
  def attributeIdOf(pathIndex: Int): Int = attributeId + pathIndex

  /** The value types in use, each listed once. Parsers are only generated for
    * these.
    */
  def usedValueTypes: Seq[ValueType] =
    paths.indices.map(valueTypeOf).distinct
}
