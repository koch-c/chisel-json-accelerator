import chisel3._
import chisel3.util._

/** Streams decoded bytes, followed by one terminal beat carrying error flags.
  */
class StringParser extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AttributeByte))
    val out = Decoupled(UInt(8.W))
    val last = Output(Bool()) // out carries the final byte of the frame
  })

  def ch(c: Char) = c.toInt.U(8.W)

  val start :: body :: escape :: after :: flags :: Nil = Enum(5)
  val state = RegInit(start)

  val char = io.in.bits.data

  // Character classes
  val isQuote = char === ch('"')
  val isBackslash = char === ch('\\')
  val isColon = char === ch(':')
  val isWhitespace =
    char === ch(' ') || char === ch('\t') || char === ch('\n') || char === ch(
      '\r'
    )
  val isControl = char < 0x20.U

  val notAStringR = RegInit(false.B)
  val badEscapeR = RegInit(false.B)

  // The character an escape letter stands for.
  val escapes = Seq(
    '"' -> '"',
    '\\' -> '\\',
    '/' -> '/',
    'b' -> '\b',
    'f' -> '\f',
    'n' -> '\n',
    'r' -> '\r',
    't' -> '\t'
  )
  val isKnownEscape =
    escapes.map { case (letter, _) => char === ch(letter) }.reduce(_ || _)
  val unescaped = MuxLookup(char, char)(escapes.map { case (letter, value) =>
    ch(letter) -> ch(value)
  })

  // Whether the current input byte puts a byte on the output.
  val emits = MuxLookup(state, false.B)(
    Seq(
      body -> (!isQuote && !isBackslash && !isControl),
      escape -> !isControl
    )
  )

  // An input byte gives at most one output byte, so the two handshakes are
  // tied together: a byte that is sent is only consumed when it is accepted.
  io.in.ready := false.B
  io.out.valid := false.B
  io.out.bits := 0.U
  io.last := false.B

  switch(state) {
    is(start, after) {
      io.in.ready := true.B
    }
    is(body, escape) {
      io.in.ready := io.out.ready || !emits
      io.out.valid := io.in.valid && emits
      io.out.bits := Mux(state === escape, unescaped, char)
    }
    is(flags) {
      io.out.valid := true.B
      io.out.bits := Cat(badEscapeR, notAStringR)
      io.last := true.B
    }
  }

  // The frame bytes that do not come from an input byte.
  when(io.out.fire) {
    switch(state) {
      is(flags) {
        // Frame sent: clear everything for the next attribute.
        state := start
        notAStringR := false.B
        badEscapeR := false.B
      }
    }
  }

  when(io.in.fire) {
    switch(state) {
      is(start) {
        when(isQuote) {
          state := body
        }.elsewhen(!isColon && !isWhitespace) {
          notAStringR := true.B
        }
      }
      is(body) {
        when(isQuote) {
          state := after
        }.elsewhen(isBackslash) {
          state := escape
        }.elsewhen(isControl) {
          notAStringR := true.B
        }
      }
      is(escape) {
        state := body
        when(!isKnownEscape) { badEscapeR := true.B }
        when(isControl) { notAStringR := true.B }
      }
      is(after) {
        when(!isWhitespace) { notAStringR := true.B }
      }
    }

    // The attribute's final byte: the string must be closed by now, either by
    // this byte or earlier.
    when(io.in.bits.last) {
      state := flags
      val closesNow = state === body && isQuote
      when(!closesNow && state =/= after) { notAStringR := true.B }
    }
  }
}
