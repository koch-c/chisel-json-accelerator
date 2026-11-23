import chisel3._
import chisel3.util._

/** Decodes one string attribute and sends it as a frame, one byte per cycle.
  *
  * The input is the byte stream of a single attribute from the AttributeLoader,
  * the same as for the NumberParser: it starts at the colon and runs up to the
  * next ',' or '}', e.g. `: "a\nb" `. The final byte has `last` set.
  *
  * The frame is streamed while the string is read, so nothing is buffered:
  *
  *   byte 0      attribute id
  *   bytes 1..n  the decoded string, without the quotes (n may be 0)
  *   byte n+1    0x00 terminator
  *   byte n+2    flags: bit 0 notAString, bit 1 badEscape
  *
  * The escapes \" \\ \/ \b \f \n \r \t are decoded. \\uXXXX is not: it sets
  * badEscape, as does any unknown escape, and the character after the
  * backslash is sent as it is. The string should then not be used.
  *
  * notAString is set when the attribute is not exactly one string: no opening
  * or closing quote, other characters than whitespace around the string, or a
  * raw control character inside it. Control characters are never sent, so the
  * terminator is the only 0x00 in a frame.
  */
class StringParser extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AttributeByte))
    val out = Decoupled(UInt(8.W))
    val last = Output(Bool()) // out carries the final byte of the frame
  })

  def ch(c: Char) = c.toInt.U(8.W)

  // header:     send the attribute id, before the first byte is consumed
  // start:      skip the colon and whitespace up to the opening quote
  // body:       inside the string
  // escape:     the previous character was a backslash
  // after:      the closing quote has been seen
  // terminator: send 0x00
  // flags:      send the flags byte
  val header :: start :: body :: escape :: after :: terminator :: flags :: Nil = Enum(7)
  val state = RegInit(header)

  val char = io.in.bits.data

  // Character classes
  val isQuote = char === ch('"')
  val isBackslash = char === ch('\\')
  val isColon = char === ch(':')
  val isWhitespace = char === ch(' ') || char === ch('\t') || char === ch('\n') || char === ch('\r')
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
  val isKnownEscape = escapes.map { case (letter, _) => char === ch(letter) }.reduce(_ || _)
  val unescaped = MuxLookup(char, char)(escapes.map { case (letter, value) => ch(letter) -> ch(value) })

  // Whether the current input byte puts a byte on the output.
  val emits = MuxLookup(state, false.B)(Seq(
    body -> (!isQuote && !isBackslash && !isControl),
    escape -> !isControl
  ))

  // An input byte gives at most one output byte, so the two handshakes are
  // tied together: a byte that is sent is only consumed when it is accepted.
  io.in.ready := false.B
  io.out.valid := false.B
  io.out.bits := 0.U
  io.last := false.B

  switch(state) {
    is(header) {
      io.out.valid := io.in.valid
      io.out.bits := io.in.bits.attributeId
    }
    is(start, after) {
      io.in.ready := true.B
    }
    is(body, escape) {
      io.in.ready := io.out.ready || !emits
      io.out.valid := io.in.valid && emits
      io.out.bits := Mux(state === escape, unescaped, char)
    }
    is(terminator) {
      io.out.valid := true.B
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
      is(header) { state := start }
      is(terminator) { state := flags }
      is(flags) {
        // Frame sent: clear everything for the next attribute.
        state := header
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
      state := terminator
      val closesNow = state === body && isQuote
      when(!closesNow && state =/= after) { notAStringR := true.B }
    }
  }
}
