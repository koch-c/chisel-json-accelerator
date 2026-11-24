import chisel3._

/** Contract between the execution stage (SPEAR-JSON Fig. 2, blocks 3 and 4)
  * and the output stage (block 5). Every port is Decoupled.
  *
  *  1. The attribute loader writes one ReorderEntry per projected attribute, in
  *     arrival order, then exactly one endOfRecord entry per record.
  *  2. Each attribute entry has exactly one value on the FIFO of its type.
  *     Values keep reorder order within a FIFO. An entry and its value may
  *     arrive in either order.
  *  3. A number that fails to parse still produces a beat, with ok = false, so
  *     the FIFOs stay aligned.
  *  4. The same id may appear twice in a record (duplicate key): last one wins.
  */
class ReorderEntry extends Bundle {
  val attributeId = UInt(8.W) // Index into OutputLayout.attributes; unused when endOfRecord.
  val endOfRecord = Bool()
}

/** One byte of a string value, quotes stripped (StringParser also decodes the
  * escapes). A zero-length string is a single beat with empty and last set.
  * ok is only read on the last beat: false means the value is not a usable
  * string, and the attribute is then absent, like a number that fails to parse.
  */
class StringBeat extends Bundle {
  val data = UInt(8.W)
  val empty = Bool() // This beat carries no byte.
  val last = Bool()
  val ok = Bool()
}

class IntValue extends Bundle {
  val value = SInt(64.W) // Always 64 bits; the output stage narrows int(32).
  val ok = Bool()
}

class FloatValue extends Bundle {
  val bits = UInt(64.W) // IEEE-754 in the attribute's declared width, right-aligned.
  val ok = Bool()
}
