/** Design-time description of the output record: the paper's attribute list
  * and filter configuration (SPEAR-JSON Sec. IV). Pure Scala, no hardware.
  */
sealed trait AttributeType {
  /** Size of this attribute's field in the output record. */
  def bytes: Int
}

/** `path::int(bits)`: two's complement, little-endian. */
final case class IntAttr(bits: Int) extends AttributeType {
  require(bits == 32 || bits == 64, "int attributes must be 32 or 64 bits")
  def bytes: Int = bits / 8
}

/** `path::float(bits)`: IEEE-754, little-endian. */
final case class FloatAttr(bits: Int) extends AttributeType {
  require(bits == 32 || bits == 64, "float attributes must be 32 or 64 bits")
  def bytes: Int = bits / 8
}

/** `path::string(bytes)`: left-aligned, NUL-padded, truncated at `bytes`. */
final case class StringAttr(bytes: Int) extends AttributeType {
  require(bytes > 0, "string attributes must be at least one byte long")
}

sealed trait CompareOp
object CompareOp {
  case object Eq extends CompareOp
  case object Lt extends CompareOp
  case object Gt extends CompareOp
  case object Le extends CompareOp
  case object Ge extends CompareOp
}

sealed trait FilterValue
final case class IntLit(value: BigInt) extends FilterValue
final case class FloatLit(value: Double) extends FilterValue
final case class StrLit(value: String) extends FilterValue

/** One `attribute op constant` term of a record filter such as `$[?(id=42)]`. */
final case class Comparison(attributeId: Int, op: CompareOp, value: FilterValue)

/** Attributes in JSONPath order; an attribute's id is its index in `attributes`.
  * A record is kept only when every comparison in `filters` holds.
  *
  * Frame on the wire: `presentBytes` of presence bitmask (bit `id % 8` of byte
  * `id / 8`), then every attribute's field at `offsets(id)`.
  */
final case class OutputLayout(
  attributes: Seq[AttributeType],
  filters: Seq[Comparison] = Nil
) {
  require(attributes.nonEmpty, "an output layout needs at least one attribute")
  require(attributes.size <= 256, "attribute ids must fit in 8 bits")

  /** Byte offset of each attribute's field within the record. */
  val offsets: Seq[Int] = attributes.scanLeft(0)(_ + _.bytes).init
  val recordBytes: Int = attributes.map(_.bytes).sum
  val presentBytes: Int = (attributes.size + 7) / 8
  val frameBytes: Int = presentBytes + recordBytes

  filters.foreach { filter =>
    require(filter.attributeId >= 0 && filter.attributeId < attributes.size,
      s"filter refers to attribute ${filter.attributeId}, but the layout has ${attributes.size}")
    (attributes(filter.attributeId), filter.value) match {
      case (IntAttr(bits), IntLit(value)) =>
        val limit = BigInt(1) << (bits - 1)
        require(value >= -limit && value < limit, s"filter constant $value does not fit int($bits)")
      case (FloatAttr(_), FloatLit(value)) =>
        require(!value.isNaN, "a filter cannot compare against NaN")
      case (FloatAttr(_), IntLit(_)) => // Compared as a float.
      case (StringAttr(bytes), StrLit(value)) =>
        require(filter.op == CompareOp.Eq, "strings only support the = comparison")
        require(value.getBytes("UTF-8").length <= bytes,
          s"filter constant \"$value\" is longer than string($bytes)")
      case (attribute, value) =>
        require(false, s"filter constant $value cannot be compared with $attribute")
    }
  }
}
