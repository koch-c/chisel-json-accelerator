/** How the selected value is delivered on the output. */
sealed trait ValueType
object ValueType {

  /** Parsed by NumberParser and sent as a 32-bit two's complement integer. */
  case object Int32 extends ValueType

  /** Parsed by NumberParser and sent as a 64-bit two's complement integer. */
  case object Int64 extends ValueType

  /** Decoded by StringParser and sent without quotes, followed by 0x00 and a
    * flags byte.
    */
  case object String extends ValueType
}

final case class FieldSpec(
    path: Seq[String],
    valueType: ValueType = ValueType.Int32,
    stringBytes: Int = 32
)

/** A filter on a projected field, identified by its full path. */
final case class FilterSpec(
    path: Seq[String],
    op: CompareOp,
    value: FilterValue
)

/** Object-key-only query. A field's index is its 8-bit attribute ID. Filters
  * are configuration only; the accelerator does not apply them yet.
  */
final case class JsonPathConfig(
    fields: Seq[FieldSpec],
    maxDepth: Int = 8,
    filters: Seq[FilterSpec] = Nil,
    numericParsers: Int = 1,
    stringParsers: Int = 1
) {
  require(fields.nonEmpty && fields.size <= 256, "expected 1 to 256 fields")
  require(maxDepth >= 1, "maxDepth must be at least one")

  val paths: Seq[Seq[String]] = fields.map(_.path)
  require(
    paths.forall(p => p.nonEmpty && p.forall(_.nonEmpty)),
    "each path must contain nonempty object keys"
  )
  require(paths.forall(_.size <= maxDepth), "paths must fit within maxDepth")
  require(paths.distinct.size == paths.size, "duplicate field paths")
  require(
    !paths.exists(p => paths.exists(q => q.size > p.size && q.startsWith(p))),
    "a scalar field cannot be a parent of another field"
  )

  val attributeIds: Map[Seq[String], Int] = paths.zipWithIndex.toMap
  require(
    filters.forall(filter => attributeIds.contains(filter.path)),
    "filters must reference projected fields"
  )
  require(numericParsers >= 1 && stringParsers >= 1)
  require(
    fields.forall(f =>
      f.valueType != ValueType.String || (f.stringBytes > 0 && f.stringBytes <= 65535)
    )
  )

  /** Distinct keys across all paths, in key-symbol code order. */
  val keys: Seq[String] = paths.flatten.distinct

  /** Stable key-symbol codes expected from the path encoder. */
  val keyIds: Map[String, Int] = keys.zipWithIndex.toMap

  /** The value types in use, each listed once. Parsers are only generated for
    * these.
    */
  val usedValueTypes: Seq[ValueType] = fields.map(_.valueType).distinct

  val numbers =
    if (fields.exists(_.valueType != ValueType.String)) numericParsers else 0
  val strings =
    if (usedValueTypes.contains(ValueType.String)) stringParsers else 0

  /** Channels are numbered with the numeric pool first, then the string pool.
    */
  val channelCount: Int = numbers + strings
  val sizes = fields.map(f =>
    if (f.valueType == ValueType.String) f.stringBytes + 2
    else if (f.valueType == ValueType.Int64) 8
    else 4
  )
  val offsets = sizes.scanLeft(0)(_ + _).init
  val statusBytes = (fields.size + 3) / 4
  val recordBytes = statusBytes + sizes.sum
}
