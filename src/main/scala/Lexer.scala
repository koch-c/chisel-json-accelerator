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
  val addr  = UInt(addrWidth.W) // byte position in the stream, Key uses its closing quote
}

/** Lexes one 8-byte word per cycle and emits all of its symbols as one packed
  * batch (lanes 0 until count, in byte order). Every byte yields at most one
  * symbol, so a batch never needs more than 8 entries. Words without symbols
  * are consumed without producing a batch.
  *
  * The byte-to-byte state is resolved across the 8 lanes in a 4-stage pipeline.
  * Each stage keeps its own carried state and never needs a later stage's, so
  * the pipeline takes a new word every cycle (latency 4 cycles):
  *  1. escapes and the in-string mask (prefix XOR of unescaped quotes),
  *  2. bracket depth per lane, enclosing container type, position in string,
  *  3. whether each string sits in key position, key matching,
  *  4. packing of the emitted symbols into the output batch.
  * All stages advance together, unless the output batch is stalled.
  *
  * Differences from a byte-serial lexer show up only on invalid JSON: a
  * backslash outside a string escapes the next byte, and once `error` is set
  * the symbol stream is unspecified.
  */
class Lexer(keys: Seq[String], maxDepth: Int = 32, addrWidth: Int = 32) extends Module {
  require(keys.nonEmpty && keys.distinct.length == keys.length, "keys must be unique")
  require(maxDepth >= 2)

  val lanes  = 8
  val symbol = new JsonSymbol(keys.length, addrWidth)

  val io = IO(new Bundle {
    val in    = Flipped(Decoupled(Vec(lanes, UInt(8.W)))) // words from InputFIFO
    val out   = Decoupled(new ChannelBatch(symbol, lanes))
    val error = Output(Bool()) // depth overflow or unbalanced brackets, sticky
  })

  def ch(c: Char) = c.toInt.U(8.W)
  def before(flags: Seq[Bool], i: Int): UInt = if (i == 0) 0.U else PopCount(flags.take(i))
  def laneRegs() = Reg(Vec(lanes, Bool()))

  val maxLen    = keys.map(_.length).max
  val idxWidth  = log2Ceil(maxLen + 2)
  val stackSize = 1 << log2Ceil(maxDepth) // power of two, so any slot index is legal
  def slot(d: UInt) = d(log2Ceil(stackSize) - 1, 0)

  val outValid = RegInit(false.B)
  val advance  = !outValid || io.out.ready // every stage moves one step
  io.in.ready := advance

  // ---- Stage 1: escapes and string mask --------------------------------
  // Escapes ignore string state (backslashes outside strings are invalid
  // JSON), which makes the string mask a plain prefix XOR of unescaped quotes.
  val inString = RegInit(false.B)
  val escaped  = RegInit(false.B) // next byte is escaped

  val esc   = (0 until lanes).scanLeft(escaped)((e, i) => io.in.bits(i) === ch('\\') && !e)
  val q     = (0 until lanes).map(i => io.in.bits(i) === ch('"') && !esc(i))
  val inStr = (0 to lanes).map(i => (inString +: q.take(i)).reduce(_ ^ _)) // before lane i

  val v1     = RegInit(false.B)
  val word1  = Reg(Vec(lanes, UInt(8.W)))
  val inStr1 = laneRegs()
  val q1     = laneRegs()
  when(advance) {
    v1     := io.in.valid
    word1  := io.in.bits
    inStr1 := VecInit(inStr.take(lanes))
    q1     := VecInit(q)
  }
  when(io.in.fire) {
    inString := inStr(lanes)
    escaped  := esc(lanes)
  }

  // ---- Stage 2: brackets and string positions --------------------------
  val isObj = Reg(Vec(stackSize, Bool())) // per open container: object or array
  val depth = RegInit(0.U(log2Ceil(maxDepth + 1).W))
  val idx   = RegInit(0.U(idxWidth.W)) // chars of the current string seen
  val error = RegInit(false.B)
  io.error := error

  val c = word1
  val kind = c.map { b =>
    val k = WireDefault(SymbolKind.Key) // Key here means "not structural"
    switch(b) {
      is(ch('{')) { k := SymbolKind.ObjOpen }
      is(ch('}')) { k := SymbolKind.ObjClose }
      is(ch('[')) { k := SymbolKind.ArrOpen }
      is(ch(']')) { k := SymbolKind.ArrClose }
      is(ch(':')) { k := SymbolKind.Colon }
      is(ch(',')) { k := SymbolKind.Comma }
    }
    k
  }
  val openQ      = (0 until lanes).map(i => q1(i) && !inStr1(i))
  val closeQ     = (0 until lanes).map(i => q1(i) && inStr1(i))
  val inside     = (0 until lanes).map(i => inStr1(i) && !closeQ(i)) // string content byte
  val isStruct   = (0 until lanes).map(i => !inStr1(i) && kind(i) =/= SymbolKind.Key)
  val opens      = (0 until lanes).map(i => !inStr1(i) && (c(i) === ch('{') || c(i) === ch('[')))
  val closes     = (0 until lanes).map(i => !inStr1(i) && (c(i) === ch('}') || c(i) === ch(']')))
  val comma      = (0 until lanes).map(i => !inStr1(i) && c(i) === ch(','))
  val braceOpen  = c.map(_ === ch('{'))
  val braceClose = c.map(_ === ch('}'))

  // Depth before each lane, offset by `lanes` so it never goes negative
  val off        = lanes
  val levelWidth = log2Ceil(maxDepth + 2 * lanes + 1)
  val level = (0 to lanes).map { i =>
    (depth.pad(levelWidth) + off.U + before(opens, i) - before(closes, i))(levelWidth - 1, 0)
  }
  // Brackets in lanes a until b leave the depth unchanged
  def balanced(a: Int, b: Int): Bool =
    if (a >= b) true.B else PopCount(opens.slice(a, b)) === PopCount(closes.slice(a, b))
  // Type of the container enclosing lane i: the latest earlier opener in this
  // word whose container is still open, otherwise the stack entry from before.
  val parentIsObj = (0 until lanes).map { i =>
    val fromStack = isObj(slot(level(i) - (off + 1).U))
    (0 until i).foldLeft(fromStack) { (acc, j) =>
      Mux(opens(j) && balanced(j + 1, i), braceOpen(j), acc)
    }
  }
  val topIsObj = (0 until lanes).map(i => level(i) =/= off.U && parentIsObj(i))

  val overflow  = (0 until lanes).map(i => opens(i) && level(i) === (maxDepth + off).U)
  val underflow = (0 until lanes).map(i => closes(i) && level(i) === off.U)
  val mismatch  = (0 until lanes).map(i => closes(i) && level(i) =/= off.U && topIsObj(i) =/= braceClose(i))

  // Key position after each structural byte: '{' sets it, ',' copies the
  // container type, the other structural bytes clear it.
  val ekAfter = (0 until lanes).map(i => Mux(opens(i), braceOpen(i), comma(i) && topIsObj(i)))

  // Chars of the current string before lane i, counted from the latest open
  // quote in this word or carried from before (saturating).
  def sat(n: Int) = (n min (maxLen + 1)).U
  val idxAt = (0 to lanes).map { i =>
    val carried = idx +& i.U
    val start   = Mux(carried > (maxLen + 1).U, sat(maxLen + 1), carried)
    (0 until i).foldLeft(start)((acc, o) => Mux(openQ(o), sat(i - 1 - o), acc))
  }
  val charOk = keys.map { k =>
    (0 until lanes).map { i =>
      k.zipWithIndex.map { case (kc, p) => idxAt(i) === p.U && c(i) === ch(kc) }.reduce(_ || _)
    }
  }
  val lenOk = keys.map(k => (0 until lanes).map(i => idxAt(i) === k.length.U))

  val v2        = RegInit(false.B)
  val kind2     = Reg(Vec(lanes, SymbolKind()))
  val isStruct2 = laneRegs()
  val ekAfter2  = laneRegs()
  val openQ2    = laneRegs()
  val closeQ2   = laneRegs()
  val inside2   = laneRegs()
  val charOk2   = Seq.fill(keys.length)(laneRegs())
  val lenOk2    = Seq.fill(keys.length)(laneRegs())
  when(advance) {
    v2        := v1
    kind2     := VecInit(kind)
    isStruct2 := VecInit(isStruct)
    ekAfter2  := VecInit(ekAfter)
    openQ2    := VecInit(openQ)
    closeQ2   := VecInit(closeQ)
    inside2   := VecInit(inside)
    for (n <- keys.indices) {
      charOk2(n) := VecInit(charOk(n))
      lenOk2(n)  := VecInit(lenOk(n))
    }
  }
  when(advance && v1) {
    for (j <- 0 until lanes) {
      when(opens(j) && level(j) >= off.U && level(j) < (maxDepth + off).U) {
        isObj(slot(level(j) - off.U)) := braceOpen(j) // later lanes win
      }
    }
    val end = level(lanes) // clamped so depth stays in range after an error
    depth := Mux(end < off.U, 0.U, Mux(end > (maxDepth + off).U, maxDepth.U, end - off.U))(depth.getWidth - 1, 0)
    idx   := idxAt(lanes)(idxWidth - 1, 0)
    when((overflow ++ underflow ++ mismatch).reduce(_ || _)) { error := true.B }
  }

  // ---- Stage 3: key position and key matching --------------------------
  val addr      = RegInit(0.U(addrWidth.W)) // byte address of lane 0
  val expectKey = RegInit(false.B) // next string is an object key
  val strIsKey  = RegInit(false.B) // current string is in key position
  val matching  = RegInit(VecInit(Seq.fill(keys.length)(false.B))) // key k still matches

  val ekBefore = (0 until lanes).scanLeft(expectKey)((ek, i) => Mux(isStruct2(i), ekAfter2(i), ek))
  val skBefore = (0 until lanes).scanLeft(strIsKey)((sk, i) => Mux(openQ2(i), ekBefore(i), sk))
  val matchBefore = keys.indices.map { n =>
    (0 until lanes).scanLeft(matching(n)) { (m, i) =>
      Mux(openQ2(i), true.B, Mux(inside2(i), m && charOk2(n)(i), m))
    }
  }
  val hits = (0 until lanes).map { i =>
    VecInit(keys.indices.map(n => closeQ2(i) && matchBefore(n)(i) && lenOk2(n)(i)))
  }
  val isKey = (0 until lanes).map(i => skBefore(i) && hits(i).asUInt.orR)
  val emit  = (0 until lanes).map(i => isKey(i) || isStruct2(i))
  val syms = (0 until lanes).map { i =>
    val s = Wire(symbol)
    s.kind  := Mux(isKey(i), SymbolKind.Key, kind2(i))
    s.keyId := PriorityEncoder(hits(i))
    s.addr  := addr + i.U
    s
  }

  val v3    = RegInit(false.B)
  val emit3 = laneRegs()
  val syms3 = Reg(Vec(lanes, symbol))
  when(advance) {
    v3    := v2
    emit3 := VecInit(emit)
    syms3 := VecInit(syms)
  }
  when(advance && v2) {
    addr      := addr + lanes.U
    expectKey := ekBefore(lanes)
    strIsKey  := skBefore(lanes)
    for (n <- keys.indices) { matching(n) := matchBefore(n)(lanes) }
  }

  // ---- Stage 4: pack the emitted symbols into the leading lanes --------
  val batch = Wire(new ChannelBatch(symbol, lanes))
  for (n <- 0 until lanes) {
    batch.entries(n) := PriorityMux((0 until lanes).map(i => (emit3(i) && before(emit3, i) === n.U) -> syms3(i)))
  }
  batch.count := PopCount(emit3)

  val outBits = Reg(new ChannelBatch(symbol, lanes))
  io.out.valid := outValid
  io.out.bits  := outBits
  when(advance) {
    outValid := v3 && emit3.asUInt.orR
    outBits  := batch
  }
}
