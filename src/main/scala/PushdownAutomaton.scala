import chisel3._
import chisel3.util.Decoupled
import chisel3.util.log2Ceil

/**
  * A small streaming JSON path matcher for paths through nested object keys.
  *
  * This first version recognizes object nesting and reports a match when the
  * final path key is followed by a colon. It does not parse values or support
  * array-index JSONPath expressions.
  */
class PushdownAutomaton(path: Seq[String], maxDepth: Int = 8) extends Module {
  require(path.nonEmpty, "path must contain at least one object key")
  require(path.forall(_.nonEmpty), "path keys must not be empty")
  require(maxDepth >= 1, "maxDepth must be at least one")

  // `deadPath` is a sentinel progress value outside the valid segment indexes
  // 0 until path.length - 1. The widths below are sized for their full ranges.
  private val deadPath = path.length
  private val progressWidth = math.max(1, log2Ceil(path.length + 1))
  private val depthWidth = math.max(1, log2Ceil(maxDepth + 1))
  private val maxKeyLength = path.map(_.length).max
  private val keyLengthWidth = math.max(1, log2Ceil(maxKeyLength + 1))
  private val stackIndexWidth = math.max(1, log2Ceil(maxDepth))
  private val keyIndexWidth = math.max(1, log2Ceil(maxKeyLength))

  // Input is one byte per Decoupled transfer. `found` stays high once matched;
  // `depth` is exposed to make the bounded stack's activity easy to inspect.
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(8.W)))
    val found = Output(Bool())
    val depth = Output(UInt(depthWidth.W))
  })

  private def ch(c: Char): UInt = c.toInt.U(8.W)

  // Each stack entry records path progress on entry to an object:
  //   0         = root object; no path key matched yet
  //   1, 2, ... = that many requested keys led to this object
  //   deadPath  = this object is not on the requested path
  // This is a register-based stack, bounded at elaboration time by maxDepth.
  // A `{` pushes one entry and a `}` pops one; excess nesting is ignored.
  val depth = RegInit(0.U(depthWidth.W))
  val stack = Reg(Vec(maxDepth, UInt(progressWidth.W)))
  // The first `{` is the root object, whose path progress starts at zero.
  stack(0) := 0.U

  // The scanner only buffers a candidate key. It compares its bytes with the
  // next expected path segment when the closing quote is received.
  val inString = RegInit(false.B)
  val escaped = RegInit(false.B)
  // Keys longer than every requested path component cannot match, so the
  // buffer saturates at maxKeyLength rather than growing with input size.
  val keyBytes = Reg(Vec(maxKeyLength, UInt(8.W)))
  val keyLength = RegInit(0.U(keyLengthWidth.W))
  // Set after a quoted string matches the expected segment; colon confirms
  // that the string was an object key, not a string-valued field.
  val candidateKey = RegInit(false.B)
  // Set after an intermediate path key's colon; the next `{` pushes its child.
  val pendingChild = RegInit(false.B)
  // Sticky result: once the full path is seen, later input does not clear it.
  val found = RegInit(false.B)

  // This initial PDA never backpressures: each offered byte is consumed on
  // every valid cycle. A future design can add backpressure for downstream work.
  io.in.ready := true.B
  io.found := found
  io.depth := depth

  // Use slot 0 for the index calculation while outside an object; key matches
  // are separately disabled when depth is zero.
  val currentStackIndex = Mux(depth === 0.U, 0.U, depth - 1.U)(stackIndexWidth - 1, 0)
  val currentProgress = stack(currentStackIndex)

  // Generate constant comparisons for every path segment. Only the segment
  // at the current object's progress can match.
  val keyMatchesExpected = path.zipWithIndex.map { case (segment, index) =>
    // Compare only the characters that belong to this compile-time segment.
    val bytesMatch = segment.zipWithIndex.map { case (char, byteIndex) =>
      keyBytes(byteIndex) === ch(char)
    }.reduce(_ && _)
    (currentProgress === index.U) &&
      (keyLength === segment.length.U) && bytesMatch
  }.reduce(_ || _)

  when(io.in.fire) {
    val c = io.in.bits

    when(inString) {
      // While inside quotes, braces and colons are ordinary string bytes.
      // The escape flag prevents an escaped quote from ending the string.
      when(escaped) {
        escaped := false.B
        when(keyLength < maxKeyLength.U) {
          keyBytes(keyLength(keyIndexWidth - 1, 0)) := c
          keyLength := keyLength + 1.U
        }
      }.elsewhen(c === ch('\\')) {
        escaped := true.B
        // JSON escape decoding is not implemented; therefore escaped path
        // spellings are conservatively rejected as potential matches.
        candidateKey := false.B
      }.elsewhen(c === ch('"')) {
        inString := false.B
        // The complete key is already in keyBytes; check its exact length and
        // contents against the segment expected in the current object.
        candidateKey := keyMatchesExpected && depth =/= 0.U
      }.otherwise {
        when(keyLength < maxKeyLength.U) {
          keyBytes(keyLength(keyIndexWidth - 1, 0)) := c
          keyLength := keyLength + 1.U
        }
      }
    }.otherwise {
      when(c === ch('"')) {
        // Start collecting a possible key; whether it is a key is confirmed
        // later when a colon follows its closing quote.
        inString := true.B
        escaped := false.B
        keyLength := 0.U
        candidateKey := false.B
      }.elsewhen(c === ch(':')) {
        // A matching non-final key means the following object may be the next
        // path component. A final matching key completes the search.
        when(candidateKey) {
          when(currentProgress === (path.length - 1).U) {
            found := true.B
          }.otherwise {
            pendingChild := true.B
          }
        }
        candidateKey := false.B
      }.elsewhen(c === ch('{')) {
        // The first object is the JSON root. Nested objects inherit progress
        // only when they follow a matching path key; others become dead ends.
        when(depth < maxDepth.U) {
          stack(depth(stackIndexWidth - 1, 0)) := Mux(
            depth === 0.U,
            0.U,
            // If this is the object belonging to the pending path key, move
            // one segment forward. Otherwise make the subtree a dead end.
            Mux(pendingChild, currentProgress + 1.U, deadPath.U)
          )
          depth := depth + 1.U
        }
        pendingChild := false.B
        candidateKey := false.B
      }.elsewhen(c === ch('}')) {
        // Leaving an object pops its path context.
        when(depth > 0.U) {
          depth := depth - 1.U
        }
        pendingChild := false.B
        candidateKey := false.B
      }.elsewhen(c =/= ch(' ') && c =/= ch('\n') && c =/= ch('\r') && c =/= ch('\t')) {
        // Any non-object value consumes a pending matched key.
        pendingChild := false.B
        candidateKey := false.B
      }
    }
  }
}
