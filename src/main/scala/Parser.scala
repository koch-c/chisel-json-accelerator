import chisel3._
import chisel3.util.{Decoupled, log2Ceil}

/** Accepts vectors of eight input bytes for JSON parsing.
  * Handles nested objects/arrays and escaped characters in strings.
  * Maybe look at the max depth, if we feel like 32 is overkill
  */
class Parser(key: String, maxDepth: Int = 32) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(Vec(8, UInt(8.W))))
    val found    = Output(Bool())
    val done = Output(Bool())
  })

  def ch(c: Char) = c.toInt.U(8.W)
  val searchKey = VecInit(key.map(ch))

  val word = Reg(Vec(8, UInt(8.W)))
  val pos  = RegInit(0.U(3.W))
  val busy = RegInit(false.B)

  val inString = RegInit(false.B) // reading str
  val escaped  = RegInit(false.B) // previous char in str was a backslash (edgecase)
  val depth    = RegInit(0.U(log2Ceil(maxDepth + 1).W)) // nesting level of {} and []
  val charsMatched      = RegInit(0.U(8.W)) // how many key chars matched
  val isMatching = RegInit(false.B) // current string still matches key
  val hit      = RegInit(false.B) // string matches the key
  val found    = RegInit(false.B)
  val done     = RegInit(false.B)
  io.done := done

  io.in.ready := !busy
  when(io.in.fire) {
    word := io.in.bits
    pos  := 0.U
    busy := true.B
  }

  val c = word(pos)
  when(busy) {
    pos := pos + 1.U
    when(pos === 7.U) { busy := false.B }

    when(!done) {
      when(inString) {
        when(c === ch('"') && !escaped) {
          inString := false.B
          hit := isMatching && charsMatched === key.length.U
        }.otherwise {
          escaped := c === ch('\\') && !escaped // \\ escapes itself
          when(isMatching && charsMatched < key.length.U && c === searchKey(charsMatched)) {
            charsMatched := charsMatched + 1.U
          }.otherwise {
            isMatching := false.B
          }
        }
      }.elsewhen(c === ch('"')) {
        inString := true.B
        escaped := false.B
        charsMatched := 0.U
        isMatching := true.B
      }.elsewhen(c === ch(':') && hit) { // only keys are followed by ':'
        found := true.B
        done := true.B
      }.elsewhen(c === ch('{') || c === ch('[')) {
        depth := depth + 1.U
      }.elsewhen(c === ch('}') || c === ch(']')) {
        depth := depth - 1.U
        when(depth === 1.U) { done := true.B } // closed the outermost object/array
      }
    }
  }

  io.found    := found
}


//future?
//Buffer for longer keys (Dynamic)
//Match only at a given depth / full paths (e.g. device.location.latitude)
//Flag an error when depth exceeds maxDepth or brackets are unbalanced
//Unicode escapes are not decoded, keys are compared byte by byte as written in the file

//Example JSON { "temperature": 25, "humidity": 60 }
