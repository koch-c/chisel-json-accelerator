import chisel3._
import chisel3.util._

/** Decimal parts of one parsed JSON number (SPEAR-JSON, Section III-C 2, Fig. 4).
  *
  * The value is  (-1)^negative * mantissa * 10^exp10.
  *   "25"       -> integer = 25, isFloat = false
  *   "12.34"    -> integer = 12, mantissa = 1234, exp10 = -2, isFloat = true
  *   "-1.25e+2" -> negative, integer = 1, mantissa = 125, exp10 = 0, isFloat = true
  * mantissa and exp10 are only meaningful when isFloat is set.
  */
class ParsedNumber extends Bundle {
  val attributeId = UInt(8.W)
  val negative = Bool()
  val integer = UInt(64.W)
  val mantissa = UInt(64.W)
  val exp10 = SInt(16.W)
  val isFloat = Bool()
  val overflow = Bool() // the number did not fit
  val notANumber = Bool() // the attribute is not a JSON number
}

/** Parses one number attribute, one byte per cycle.
  *
  * The input is the byte stream of a single attribute from the AttributeLoader.
  * It is not just the number: it starts at the colon and runs up to the next
  * ',' or '}', e.g. ": 25" or ":-1.25e+2  ". The final byte has `last` set.
  * Exactly one ParsedNumber is produced per attribute.
  *
  * Not part of this module (yet): the IEEE754 conversion, several parsers in
  * parallel behind an arbiter, and the connection into Accelerator.
  */
class NumberParser extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AttributeByte))
    val out = Decoupled(new ParsedNumber)
  })

  def ch(c: Char) = c.toInt.U(8.W)

  // States of Fig. 4(a). `done` is not in the paper: it holds the result
  // until the consumer has accepted it.
  val start :: sign :: int :: fract :: expSign :: exp :: done :: Nil = Enum(7)
  val state = RegInit(start)

  val char = io.in.bits.data

  // Character classes
  val isDigit = char >= ch('0') && char <= ch('9')
  val isMinus = char === ch('-')
  val isPlus = char === ch('+')
  val isPoint = char === ch('.')
  val isExp = char === ch('e') || char === ch('E')
  val isColon = char === ch(':')
  val isWhitespace = char === ch(' ') || char === ch('\t') || char === ch('\n') || char === ch('\r')

  // Registers of Fig. 4(b), with the paper's names.
  val accR = RegInit(0.U(64.W))
  val intR = RegInit(0.U(64.W))
  val mantissaR = RegInit(0.U(64.W))
  val exp10R = RegInit(0.S(16.W))
  val signR = RegInit(false.B)
  val eSignR = RegInit(false.B)
  val isFloatR = RegInit(false.B)
  val attributeIdR = RegInit(0.U(8.W))
  val lastR = RegInit(false.B) // the attribute's final byte has been taken
  val overflowR = RegInit(false.B)
  val notANumberR = RegInit(false.B)

  // Handshake: no new bytes while a result is waiting to be picked up.
  io.in.ready := state =/= done && !lastR
  io.out.valid := state === done

  io.out.bits.attributeId := attributeIdR
  io.out.bits.negative := signR
  io.out.bits.integer := intR
  io.out.bits.mantissa := mantissaR
  io.out.bits.exp10 := exp10R
  io.out.bits.isFloat := isFloatR
  io.out.bits.overflow := overflowR
  io.out.bits.notANumber := notANumberR


  // The written exponent is accumulated in accR as an unsigned value. Apply its
  // sign and add it to the fraction count that exp10R already holds.
  val expMagnitude = accR(14, 0).zext
  val exp10WithExponent = Mux(eSignR, exp10R - expMagnitude, exp10R + expMagnitude)

// Control path based on figure 4a in the paper.
  when(io.in.fire) {
    switch(state) {
      is(start) { //only move if the start of a number is registered.
        when(isMinus) {
          state := sign
          signR := true.B // ps signal
        }.elsewhen(isDigit) {
          state := int
        }
      }
      is(sign) {
        when(isDigit) { state := int }
      }
      is(int) {
        when(isPoint) {
          state := fract
          intR := accR // si
          isFloatR := true.B
        }.elsewhen(isExp) {
          state := expSign
          intR := accR // si
          mantissaR := accR // sm
          accR := 0.U // free the accumulator for the exponent
          isFloatR := true.B
        }
      }
      is(fract) {
        when(isDigit) {
          exp10R := exp10R - 1.S // de
        }.elsewhen(isExp) {
          state := expSign
          mantissaR := accR // sm
          accR := 0.U // free the accumulator for the exponent
        }
      }
      is(expSign) {
        // First character after 'e' needs to be a sign or the first exponent digit.
        when(isMinus) {
          state := exp
          eSignR := true.B // pes
        }.elsewhen(isPlus || isDigit) {
          state := exp
        }
      }
    }    
  }


  // Accumulator (signal `pn` in the figure): for every digit, shift the old
  // value one decimal place and add the new digit. So it reads it left to right.
  val digit = char(3, 0) // '0'..'9' are 0x30..0x39, so the low nibble is the value
  val accNext = (accR << 3) +& (accR << 1) +& digit
  when(io.in.fire && isDigit) {
    accR := accNext
    when((accNext >> 64).orR) { overflowR := true.B }
  }


  // Which characters a number may contain at this point. Anything else means
  // the attribute is not a number.
  val legal = MuxLookup(state, false.B)(Seq(
    start -> (isColon || isWhitespace || isMinus || isDigit),
    sign -> isDigit,
    int -> (isDigit || isPoint || isExp || isWhitespace),
    fract -> (isDigit || isExp || isWhitespace),
    expSign -> (isDigit || isPlus || isMinus),
    exp -> (isDigit || isWhitespace)
  ))
  when(io.in.fire && !legal) { notANumberR := true.B }

  // Checks what attribute we are loading at every cycle and when the final byte have been taken.
  when(io.in.fire) {
    attributeIdR := io.in.bits.attributeId
    when(io.in.bits.last) { lastR := true.B }
  }

  // When done with the last byte we go one more cycle and then store what the acc holds.
  when(lastR) {
    lastR := false.B
    state := done
    switch(state) {
      is(int) { intR := accR }
      is(fract) { mantissaR := accR }
      is(exp) {
        exp10R := exp10WithExponent
        when(accR(63, 14).orR) { overflowR := true.B }
      }
      // Ended before any digit, right after a sign, or right after the 'e'.
      is(start, sign, expSign) { notANumberR := true.B }
    }
  }

  // Result accepted: clear everything for the next number.
  when(io.out.fire) {
    state := start
    accR := 0.U
    intR := 0.U
    mantissaR := 0.U
    exp10R := 0.S
    signR := false.B
    eSignR := false.B
    isFloatR := false.B
    overflowR := false.B
    notANumberR := false.B
  }

}
