import chisel3._
import chisel3.util._

/** Kinds of symbols the lexer emits. Everything else in the JSON
  * (values, whitespace, keys we do not care about) is dropped.
  */
object SymbolKind extends ChiselEnum {
  val ObjOpen, ObjClose, ArrOpen, ArrClose, Colon, Comma, Key = Value
}

/** One symbol for the pushdown automaton. */
class JsonSymbol(numKeys: Int, addrWidth: Int = 12) extends Bundle {
  val kind  = SymbolKind()
  val keyId = UInt(log2Ceil(numKeys max 2).W) // which of the path keys, only valid for Key
  val addr  = UInt(addrWidth.W) // input buffer byte address, Key uses its closing quote
}

/** addr is the byte address of c in the input buffer (512 words of 8 bytes).
  * Bits 11-3 are the word address, bits 2-0 the byte offset, so it can be
  * passed on to the attribute loader as is. It wraps after byte 4095,
  * the same way the buffer is written.
  */
class Lexer(keys: Seq[String], maxDepth: Int = 32, addrWidth: Int = 12) extends Module {
  require(keys.nonEmpty && keys.distinct.length == keys.length, "keys must be unique")
  require(maxDepth >= 2)

  val io = IO(new Bundle {
    val in    = Flipped(Decoupled(Vec(8, UInt(8.W)))) // words from the Streamer
    val out   = Decoupled(new JsonSymbol(keys.length, addrWidth))
    val error = Output(Bool()) // depth overflow or unbalanced brackets, sticky
  })

  def ch(c: Char) = c.toInt.U(8.W)

  val maxLen   = keys.map(_.length).max
  val idxWidth = log2Ceil(maxLen + 2) // counts up to maxLen + 1, "too long"
  // Each key padded with zeros so any idx is a legal index
  val keyRoms = keys.map(k => VecInit(k.map(ch).padTo(1 << idxWidth, 0.U(8.W))))

  val addr      = RegInit(0.U(addrWidth.W))
  val inString  = RegInit(false.B)
  val escaped   = RegInit(false.B) // previous char in string was a backslash
  val strIsKey  = RegInit(false.B) // current string is in key position
  val idx       = RegInit(0.U(idxWidth.W)) // chars of the current string seen
  val matching  = RegInit(VecInit(Seq.fill(keys.length)(false.B))) // key k still matches
  val expectKey = RegInit(false.B) // next string is an object key
  val isObj     = Reg(Vec(maxDepth, Bool())) // per open container: object or array
  val depth     = RegInit(0.U(log2Ceil(maxDepth + 1).W))
  val error     = RegInit(false.B)
  io.error := error

  val word = Reg(Vec(8, UInt(8.W)))
  val pos  = RegInit(0.U(3.W)) // byte of the word being lexed
  val busy = RegInit(false.B) // word holds bytes not lexed yet

  val c        = word(pos)
  def slot(d: UInt) = d(log2Ceil(maxDepth) - 1, 0) // depth always fits when used as an index
  val topIsObj = depth =/= 0.U && isObj(slot(depth - 1.U))

  // Which symbol (if any) does this byte produce?
  val closeQuote = inString && c === ch('"') && !escaped
  val hits       = VecInit(keys.zipWithIndex.map { case (k, i) => matching(i) && idx === k.length.U })
  val isKey      = closeQuote && strIsKey && hits.asUInt.orR

  val kind = WireDefault(SymbolKind.Key) // Key here means "not structural"
  switch(c) {
    is(ch('{')) { kind := SymbolKind.ObjOpen }
    is(ch('}')) { kind := SymbolKind.ObjClose }
    is(ch('[')) { kind := SymbolKind.ArrOpen }
    is(ch(']')) { kind := SymbolKind.ArrClose }
    is(ch(':')) { kind := SymbolKind.Colon }
    is(ch(',')) { kind := SymbolKind.Comma }
  }
  val isStructural = !inString && kind =/= SymbolKind.Key
  val emit         = isKey || isStructural

  val step = busy && (io.out.ready || !emit) // byte c is done this cycle

  io.out.valid      := busy && emit
  io.out.bits.kind  := Mux(isKey, SymbolKind.Key, kind)
  io.out.bits.keyId := PriorityEncoder(hits)
  io.out.bits.addr  := addr
  io.in.ready       := !busy || (step && pos === 7.U) // take the next word as the last byte leaves

  when(step) {
    pos := pos + 1.U
    when(pos === 7.U) { busy := false.B }
  }
  when(io.in.fire) {
    word := io.in.bits
    pos  := 0.U
    busy := true.B
  }

  when(step) {
    addr := addr + 1.U

    when(inString) {
      when(closeQuote) {
        inString := false.B
      }.otherwise {
        escaped := c === ch('\\') && !escaped // \\ escapes itself
        for ((k, i) <- keys.zipWithIndex) {
          matching(i) := matching(i) && idx < k.length.U && c === keyRoms(i)(idx)
        }
        when(idx =/= (maxLen + 1).U) { idx := idx + 1.U }
      }
    }.elsewhen(c === ch('"')) {
      inString := true.B
      escaped  := false.B
      strIsKey := expectKey
      idx      := 0.U
      matching.foreach(_ := true.B)
    }.elsewhen(c === ch('{') || c === ch('[')) {
      when(depth === maxDepth.U) {
        error := true.B
      }.otherwise {
        isObj(slot(depth)) := c === ch('{')
        depth        := depth + 1.U
      }
      expectKey := c === ch('{')
    }.elsewhen(c === ch('}') || c === ch(']')) {
      when(depth === 0.U || topIsObj =/= (c === ch('}'))) { error := true.B }
      when(depth =/= 0.U) { depth := depth - 1.U }
      expectKey := false.B
    }.elsewhen(c === ch(',')) {
      expectKey := topIsObj
    }.elsewhen(c === ch(':')) {
      expectKey := false.B
    }
  }
}
