import chisel3._
import chisel3.util._

/** Record filter of the output stage (SPEAR-JSON Sec. III-D): one comparator
  * per Comparison, all ANDed. A comparison on an absent attribute is false.
  *
  * Numbers are compared through an unsigned key that preserves their order, so
  * ints and floats share one comparator shape.
  */
object SelectionLogic {
  def keep(
      layout: OutputLayout,
      fields: Vec[UInt],
      present: Vec[Bool],
      truncated: Vec[Bool]
  ): Bool = {
    val terms = layout.filters.map { filter =>
      val id = filter.attributeId
      val attribute = layout.attributes(id)
      val offset = layout.offsets(id)
      // Byte 0 of a field is its least significant byte.
      val raw =
        Cat((0 until attribute.bytes).reverse.map(i => fields(offset + i)))
      val matches = (attribute, filter.value) match {
        case (IntAttr(bits), IntLit(value)) =>
          compare(filter.op, intKey(raw, bits), intKey(value, bits).U(bits.W))
        case (FloatAttr(bits), FloatLit(value)) =>
          compareFloat(filter.op, raw, bits, value)
        case (FloatAttr(bits), IntLit(value)) =>
          compareFloat(filter.op, raw, bits, value.toDouble)
        case (StringAttr(bytes), StrLit(value)) =>
          // A truncated string only shares a prefix with the constant.
          raw === stringConstant(value).U((8 * bytes).W) && !truncated(id)
        case (other, value) =>
          throw new IllegalArgumentException(
            s"$value cannot be compared with $other"
          )
      }
      present(id) && matches
    }
    terms.foldLeft(true.B)(_ && _)
  }

  private def compare(op: CompareOp, key: UInt, constant: UInt): Bool =
    op match {
      case CompareOp.Eq => key === constant
      case CompareOp.Lt => key < constant
      case CompareOp.Gt => key > constant
      case CompareOp.Le => key <= constant
      case CompareOp.Ge => key >= constant
    }

  private def signBit(bits: Int): BigInt = BigInt(1) << (bits - 1)
  private def mask(bits: Int): BigInt = (BigInt(1) << bits) - 1

  // Two's complement order becomes unsigned order when the sign bit is flipped.
  private def intKey(raw: UInt, bits: Int): UInt = raw ^ signBit(bits).U(bits.W)
  private def intKey(value: BigInt, bits: Int): BigInt =
    (value & mask(bits)) ^ signBit(bits)

  // IEEE-754 is sign-magnitude: set the sign bit of non-negative values and
  // invert negative ones. -0 is treated as +0.
  private def floatKey(raw: UInt, bits: Int): UInt = {
    val negative = raw(bits - 1) && raw(bits - 2, 0).orR
    Mux(negative, ~raw, raw | signBit(bits).U(bits.W))
  }
  private def floatKey(raw: BigInt, bits: Int): BigInt = {
    val negative = raw.testBit(bits - 1) && (raw & (signBit(bits) - 1)) != 0
    if (negative) ~raw & mask(bits) else raw | signBit(bits)
  }

  private def compareFloat(
      op: CompareOp,
      raw: UInt,
      bits: Int,
      constant: Double
  ): Bool = {
    val mantissaBits = if (bits == 32) 23 else 52
    val constantBits =
      if (bits == 32)
        BigInt(java.lang.Float.floatToIntBits(constant.toFloat)) & mask(32)
      else BigInt(java.lang.Double.doubleToLongBits(constant)) & mask(64)
    // NaN compares false with everything.
    val isNaN = raw(bits - 2, mantissaBits).andR && raw(mantissaBits - 1, 0).orR
    compare(
      op,
      floatKey(raw, bits),
      floatKey(constantBits, bits).U(bits.W)
    ) && !isNaN
  }

  private def stringConstant(value: String): BigInt =
    value
      .getBytes("UTF-8")
      .zipWithIndex
      .map { case (byte, i) => BigInt(byte & 0xff) << (8 * i) }
      .sum
}
