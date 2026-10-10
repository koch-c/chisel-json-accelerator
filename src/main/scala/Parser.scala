import chisel3._
import chisel3.util.Decoupled

/** Accepts vectors of eight input bytes for JSON parsing. */
class Parser(key: String) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(Vec(8, UInt(8.W))))
    val found    = Output(Bool())
    val notFound = Output(Bool())
  })

  def ch(c: Char) = c.toInt.U(8.W)
  val searchKey = VecInit(key.map(ch))

  val word = Reg(Vec(8, UInt(8.W)))
  val pos  = RegInit(0.U(3.W))
  val busy = RegInit(false.B)

  val inString = RegInit(false.B) // reading str
  val charsMatched      = RegInit(0.U(8.W)) // how many key chars matched
  val isMatching = RegInit(false.B) // current string still matches key
  val hit      = RegInit(false.B) // string matches the key
  val found    = RegInit(false.B)
  val done     = RegInit(false.B)

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
        when(c === ch('"')) { 
          inString := false.B
          hit := isMatching && charsMatched === key.length.U
        }.elsewhen(isMatching && charsMatched < key.length.U && c === searchKey(charsMatched)) {
          charsMatched := charsMatched + 1.U
        }.otherwise {
          isMatching := false.B
        }
      }.elsewhen(c === ch('"')) { 
        inString := true.B
        charsMatched := 0.U
        isMatching := true.B
      }.elsewhen(c === ch(':') && hit) {
        found := true.B
      }.elsewhen(c === ch('}')) { 
        done := true.B
      }
    }
  }

  io.found    := found
  io.notFound := done && !found
}


//future?
//Buffer for longer keys (Dynamic)
//Ability to distinguish between keys and values 
//(maybe flags based on characters ":" "{}")
//Should handle nested JSON files

//Example JSON { "temperature": 25, "humidity": 60 }
