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

final case class FieldSpec(
    path: Seq[String],
    valueType: ValueType = ValueType.Raw
)

/** Object-key-only query. A field's index is its 8-bit attribute ID. */
final case class JsonPathConfig(fields: Seq[FieldSpec], maxDepth: Int = 8) {
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

  /** Distinct keys across all paths, in key-symbol code order. */
  val keys: Seq[String] = paths.flatten.distinct

  /** Stable key-symbol codes expected from the path encoder. */
  val keyIds: Map[String, Int] = keys.zipWithIndex.toMap
}
