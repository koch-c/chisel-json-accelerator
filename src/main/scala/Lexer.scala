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
  */
class Lexer(keys: Seq[String], maxDepth: Int = 32, addrWidth: Int = 32) extends Module {
  require(keys.nonEmpty && keys.distinct.length == keys.length, "keys must be unique")
  require(maxDepth >= 2)

  val lanes      = 8
  val symbolType = new JsonSymbol(keys.length, addrWidth)

  val io = IO(new Bundle {
    val in    = Flipped(Decoupled(Vec(lanes, UInt(8.W)))) // words from InputFIFO
    val out   = Decoupled(new ChannelBatch(symbolType, lanes))
    val error = Output(Bool()) // depth overflow or unbalanced brackets, sticky
  })

  def ascii(c: Char) = c.toInt.U(8.W)
  /** How many of `flags` are set in the lanes before lane i. */
  def countBefore(flags: Seq[Bool], i: Int): UInt = if (i == 0) 0.U else PopCount(flags.take(i))
  def laneFlagRegs() = Reg(Vec(lanes, Bool()))

  val maxKeyLen      = keys.map(_.length).max
  val charCountWidth = log2Ceil(maxKeyLen + 2)
  val stackSize      = 1 << log2Ceil(maxDepth) // power of two, so any slot index is legal
  def stackSlot(depth: UInt) = depth(log2Ceil(stackSize) - 1, 0)

  val outValid = RegInit(false.B)
  val advance  = !outValid || io.out.ready // every stage moves one step
  io.in.ready := advance

  // ---- Stage 1: escapes and string mask --------------------------------
  // Escapes ignore string state (backslashes outside strings are invalid
  // JSON), which makes the string mask a plain prefix XOR of unescaped quotes.
  val inString      = RegInit(false.B) // carried: the next word starts inside a string
  val pendingEscape = RegInit(false.B) // carried: the next word's first byte is escaped

  val inBytes   = io.in.bits
  val escapedAt = (0 until lanes).scanLeft(pendingEscape)((e, i) => inBytes(i) === ascii('\\') && !e)
  val isQuote   = (0 until lanes).map(i => inBytes(i) === ascii('"') && !escapedAt(i)) // unescaped quote
  val inStringBefore = (0 to lanes).map(i => (inString +: isQuote.take(i)).reduce(_ ^ _))

  val stage1Valid    = RegInit(false.B)
  val stage1Bytes    = Reg(Vec(lanes, UInt(8.W)))
  val stage1InString = laneFlagRegs()
  val stage1IsQuote  = laneFlagRegs()
  when(advance) {
    stage1Valid    := io.in.valid
    stage1Bytes    := inBytes
    stage1InString := VecInit(inStringBefore.take(lanes))
    stage1IsQuote  := VecInit(isQuote)
  }
  when(io.in.fire) {
    inString      := inStringBefore(lanes)
    pendingEscape := escapedAt(lanes)
  }

  // ---- Stage 2: brackets and string positions --------------------------
  val stackIsObject = Reg(Vec(stackSize, Bool())) // per open container: object or array
  val depth         = RegInit(0.U(log2Ceil(maxDepth + 1).W))
  val charsSeen     = RegInit(0.U(charCountWidth.W)) // chars of the current string so far
  val error         = RegInit(false.B)
  io.error := error

  val bytes = stage1Bytes
  val structKind = bytes.map { b =>
    val k = WireDefault(SymbolKind.Key) // Key here means "not structural"
    switch(b) {
      is(ascii('{')) { k := SymbolKind.ObjOpen }
      is(ascii('}')) { k := SymbolKind.ObjClose }
      is(ascii('[')) { k := SymbolKind.ArrOpen }
      is(ascii(']')) { k := SymbolKind.ArrClose }
      is(ascii(':')) { k := SymbolKind.Colon }
      is(ascii(',')) { k := SymbolKind.Comma }
    }
    k
  }
  val outside      = (0 until lanes).map(i => !stage1InString(i)) // byte is not inside a string
  val openQuote    = (0 until lanes).map(i => stage1IsQuote(i) && outside(i))
  val closeQuote   = (0 until lanes).map(i => stage1IsQuote(i) && !outside(i))
  val stringBody   = (0 until lanes).map(i => !outside(i) && !closeQuote(i)) // string content byte
  val isStructural = (0 until lanes).map(i => outside(i) && structKind(i) =/= SymbolKind.Key)
  val isOpener     = (0 until lanes).map(i => outside(i) && (bytes(i) === ascii('{') || bytes(i) === ascii('[')))
  val isCloser     = (0 until lanes).map(i => outside(i) && (bytes(i) === ascii('}') || bytes(i) === ascii(']')))
  val isComma      = (0 until lanes).map(i => outside(i) && bytes(i) === ascii(','))
  val isBraceOpen  = bytes.map(_ === ascii('{'))
  val isBraceClose = bytes.map(_ === ascii('}'))

  // Depth before each lane, offset by `lanes` so it never goes negative
  val depthOffset = lanes
  val depthWidth  = log2Ceil(maxDepth + 2 * lanes + 1)
  val depthBefore = (0 to lanes).map { i =>
    (depth.pad(depthWidth) + depthOffset.U + countBefore(isOpener, i) - countBefore(isCloser, i))(depthWidth - 1, 0)
  }
  // Brackets in lanes a until b leave the depth unchanged
  def balanced(a: Int, b: Int): Bool =
    if (a >= b) true.B else PopCount(isOpener.slice(a, b)) === PopCount(isCloser.slice(a, b))
  // Type of the container enclosing lane i: the latest earlier opener in this
  // word whose container is still open, otherwise the stack entry from before.
  val parentIsObject = (0 until lanes).map { i =>
    val fromStack = stackIsObject(stackSlot(depthBefore(i) - (depthOffset + 1).U))
    (0 until i).foldLeft(fromStack) { (acc, j) =>
      Mux(isOpener(j) && balanced(j + 1, i), isBraceOpen(j), acc)
    }
  }
  val insideObject = (0 until lanes).map(i => depthBefore(i) =/= depthOffset.U && parentIsObject(i))

  val overflow  = (0 until lanes).map(i => isOpener(i) && depthBefore(i) === (maxDepth + depthOffset).U)
  val underflow = (0 until lanes).map(i => isCloser(i) && depthBefore(i) === depthOffset.U)
  val mismatch  = (0 until lanes).map { i =>
    isCloser(i) && depthBefore(i) =/= depthOffset.U && insideObject(i) =/= isBraceClose(i)
  }

  // Key position after each structural byte: '{' sets it, ',' copies the
  // container type, the other structural bytes clear it.
  val expectKeyAfter = (0 until lanes).map { i =>
    Mux(isOpener(i), isBraceOpen(i), isComma(i) && insideObject(i))
  }

  // Chars of the current string before lane i, counted from the latest open
  // quote in this word or carried from before (saturating at maxKeyLen + 1).
  def saturate(n: Int) = (n min (maxKeyLen + 1)).U
  val charsSeenBefore = (0 to lanes).map { i =>
    val continued = charsSeen +& i.U
    val start     = Mux(continued > (maxKeyLen + 1).U, saturate(maxKeyLen + 1), continued)
    (0 until i).foldLeft(start)((acc, o) => Mux(openQuote(o), saturate(i - 1 - o), acc))
  }
  // keyCharMatches(k)(i): the byte in lane i is the right char of key k at its position
  val keyCharMatches = keys.map { key =>
    (0 until lanes).map { i =>
      key.zipWithIndex.map { case (kc, p) => charsSeenBefore(i) === p.U && bytes(i) === ascii(kc) }.reduce(_ || _)
    }
  }
  // keyLengthMatches(k)(i): a string closing in lane i has the length of key k
  val keyLengthMatches = keys.map(key => (0 until lanes).map(i => charsSeenBefore(i) === key.length.U))

  val stage2Valid            = RegInit(false.B)
  val stage2Kind             = Reg(Vec(lanes, SymbolKind()))
  val stage2IsStructural     = laneFlagRegs()
  val stage2ExpectKeyAfter   = laneFlagRegs()
  val stage2OpenQuote        = laneFlagRegs()
  val stage2CloseQuote       = laneFlagRegs()
  val stage2StringBody       = laneFlagRegs()
  val stage2KeyCharMatches   = Seq.fill(keys.length)(laneFlagRegs())
  val stage2KeyLengthMatches = Seq.fill(keys.length)(laneFlagRegs())
  when(advance) {
    stage2Valid          := stage1Valid
    stage2Kind           := VecInit(structKind)
    stage2IsStructural   := VecInit(isStructural)
    stage2ExpectKeyAfter := VecInit(expectKeyAfter)
    stage2OpenQuote      := VecInit(openQuote)
    stage2CloseQuote     := VecInit(closeQuote)
    stage2StringBody     := VecInit(stringBody)
    for (k <- keys.indices) {
      stage2KeyCharMatches(k)   := VecInit(keyCharMatches(k))
      stage2KeyLengthMatches(k) := VecInit(keyLengthMatches(k))
    }
  }
  when(advance && stage1Valid) {
    for (j <- 0 until lanes) {
      when(isOpener(j) && depthBefore(j) >= depthOffset.U && depthBefore(j) < (maxDepth + depthOffset).U) {
        stackIsObject(stackSlot(depthBefore(j) - depthOffset.U)) := isBraceOpen(j) // later lanes win
      }
    }
    val end = depthBefore(lanes) // clamped so depth stays in range after an error
    depth := Mux(end < depthOffset.U, 0.U,
      Mux(end > (maxDepth + depthOffset).U, maxDepth.U, end - depthOffset.U))(depth.getWidth - 1, 0)
    charsSeen := charsSeenBefore(lanes)(charCountWidth - 1, 0)
    when((overflow ++ underflow ++ mismatch).reduce(_ || _)) { error := true.B }
  }

  // ---- Stage 3: key position and key matching --------------------------
  val wordAddr        = RegInit(0.U(addrWidth.W)) // byte address of lane 0
  val expectKey       = RegInit(false.B) // next string is an object key
  val stringIsKey     = RegInit(false.B) // current string is in key position
  val keyStillMatches = RegInit(VecInit(Seq.fill(keys.length)(false.B))) // per key

  val expectKeyBefore = (0 until lanes).scanLeft(expectKey) { (ek, i) =>
    Mux(stage2IsStructural(i), stage2ExpectKeyAfter(i), ek)
  }
  val stringIsKeyBefore = (0 until lanes).scanLeft(stringIsKey) { (sk, i) =>
    Mux(stage2OpenQuote(i), expectKeyBefore(i), sk)
  }
  val keyMatchBefore = keys.indices.map { k =>
    (0 until lanes).scanLeft(keyStillMatches(k)) { (m, i) =>
      Mux(stage2OpenQuote(i), true.B, Mux(stage2StringBody(i), m && stage2KeyCharMatches(k)(i), m))
    }
  }
  // keyHit(i)(k): the string closing in lane i is exactly key k
  val keyHit = (0 until lanes).map { i =>
    VecInit(keys.indices.map(k => stage2CloseQuote(i) && keyMatchBefore(k)(i) && stage2KeyLengthMatches(k)(i)))
  }
  val isKey = (0 until lanes).map(i => stringIsKeyBefore(i) && keyHit(i).asUInt.orR)
  val emit  = (0 until lanes).map(i => isKey(i) || stage2IsStructural(i))
  val symbols = (0 until lanes).map { i =>
    val s = Wire(symbolType)
    s.kind  := Mux(isKey(i), SymbolKind.Key, stage2Kind(i))
    s.keyId := PriorityEncoder(keyHit(i))
    s.addr  := wordAddr + i.U
    s
  }

  val stage3Valid   = RegInit(false.B)
  val stage3Emit    = laneFlagRegs()
  val stage3Symbols = Reg(Vec(lanes, symbolType))
  when(advance) {
    stage3Valid   := stage2Valid
    stage3Emit    := VecInit(emit)
    stage3Symbols := VecInit(symbols)
  }
  when(advance && stage2Valid) {
    wordAddr    := wordAddr + lanes.U
    expectKey   := expectKeyBefore(lanes)
    stringIsKey := stringIsKeyBefore(lanes)
    for (k <- keys.indices) { keyStillMatches(k) := keyMatchBefore(k)(lanes) }
  }

  // ---- Stage 4: pack the emitted symbols into the leading lanes --------
  val batch = Wire(new ChannelBatch(symbolType, lanes))
  for (n <- 0 until lanes) {
    batch.entries(n) := PriorityMux((0 until lanes).map { i =>
      (stage3Emit(i) && countBefore(stage3Emit, i) === n.U) -> stage3Symbols(i)
    })
  }
  batch.count := PopCount(stage3Emit)

  val outBits = Reg(new ChannelBatch(symbolType, lanes))
  io.out.valid := outValid
  io.out.bits  := outBits
  when(advance) {
    outValid := stage3Valid && stage3Emit.asUInt.orR
    outBits  := batch
  }
}
