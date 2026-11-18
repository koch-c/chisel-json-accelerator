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
  // TODO(widths): 64 bits is a placeholder. How many decimal digits fit in 64 bits,
  // and what should happen with a number that has more digits than that?
  val integer = UInt(64.W)
  val mantissa = UInt(64.W)
  // TODO(widths): which range of exponents do we need to represent?
  val exp10 = SInt(16.W)
  val isFloat = Bool()
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
  val isSkippable = char === ch(':') || char === ch('\t') || char === ch('\n') || char === ch('\r') || char === ch(' ') //Potentially exclude \n since accelerator might not send line endings

  // Registers of Fig. 4(b), with the paper's names.
  val accR = RegInit(0.U(64.W))
  val intR = RegInit(0.U(64.W))
  val mantissaR = RegInit(0.U(64.W))
  val exp10R = RegInit(0.S(16.W))
  val signR = RegInit(false.B)
  val eSignR = RegInit(false.B)
  val isFloatR = RegInit(false.B)
  val attributeIdR = RegInit(0.U(8.W))

  // Handshake: no new bytes while a result is waiting to be picked up.
  io.in.ready := state =/= done
  io.out.valid := state === done

  io.out.bits.attributeId := attributeIdR
  io.out.bits.negative := signR
  io.out.bits.integer := intR
  io.out.bits.mantissa := mantissaR
  io.out.bits.exp10 := exp10R
  io.out.bits.isFloat := isFloatR

  // TODO 2: control path, Fig. 4(a).
  // Fill in the transitions. The FSM should only move when a byte is actually
  // transferred (io.in.fire).
  // Hints:
  //  - Each edge in the figure is labelled "characters / control signals". Start
  //    with only the state changes and add the register updates in TODO 3 to 6.
  //  - Compare the figure with the JSON grammar: is the sign optional? Can a
  //    number go from `int` straight to `exp`?
  //  - `start` also has to get past the leading ':' and whitespace.
  switch(state) {
    is(start) {



    }
    is(sign) {
    }
    is(int) {
    }
    is(fract) {
    }
    is(expSign) {
    }
    is(exp) {
    }
    is(done) {
    }
  }

  // TODO 3: accumulator (signal `pn` in the figure).
  // For every digit, accR becomes "the old value shifted one decimal place, plus
  // the new digit".
  // Hints:
  //  - What is the numeric value of the ASCII character '7'?
  //  - Check the width of the result of a multiplication in Chisel.
  //  - Don't forget the sign (`ps`): where is it stored?

  when(io.in.fire && isDigit)
  {
      //When adding the digit to the accummolator we multiply by 10 and then add the digit.
      val multiply = accR << 3.U + 2.U
      val addDigit = multiply & char

      accR = addDigit

  }

  // TODO 4: saving the accumulator (signals `si` and `sm`).
  // On which edges is accR copied to intR, and on which to mantissaR?
  // Hint: read the sentence about "the transition to the exponent state" in the
  // paper. Why does accR have to be free again at that point, and why is it NOT
  // cleared when the point is seen?

  // TODO 5: fraction (signal `de`).
  // "12.34" is accumulated as 1234. What has to happen to exp10R for each digit
  // in `fract` so that mantissa * 10^exp10 is still 12.34?

  // TODO 6: exponent (signal `pes` and the exp states).
  // The exponent digits are accumulated in accR like any other digits.
  // Hints:
  //  - Where does the exponent's sign go, and when is it applied?
  //  - The final exp10 combines two things: the fraction count from TODO 5 and
  //    the parsed exponent. Where is the cheapest place to add them?

  // TODO 7: termination and the next number.
  //  a) The number ends on a character that has no edge in the figure, or on
  //     the `last` byte. Which registers must be written, depending on the state
  //     we are in at that moment?
  //  b) There may be trailing whitespace after the number (":-1.25e+2  "). These
  //     bytes still have to be consumed up to and including `last` before the
  //     next attribute arrives. Decide in which state that happens.
  //  c) Go to `done` once per attribute, and back to `start` on io.out.fire.
  //     Which registers must be cleared before the next number?
}
