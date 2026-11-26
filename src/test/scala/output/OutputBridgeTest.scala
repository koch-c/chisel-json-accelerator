import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable

/** Parts 3, 4 and 5 wired as in OutputBridge.scala, with the PDA played by the
  * test. stringParser builds the string parser and returns its in, out and last
  * ports; without one, string attributes are dropped.
  */
class ExecutionHarness(
    layout: OutputLayout,
    stringParser: Option[
      () => (DecoupledIO[AttributeByte], DecoupledIO[UInt], Bool)
    ] = None
) extends Module {
  val io = IO(new Bundle {
    val writeEnable = Input(Bool())
    val writeAddress = Input(UInt(9.W))
    val writeData = Input(UInt(64.W))
    val descriptor = Flipped(Decoupled(new AttributeDescriptor))
    val endOfRecord = Input(Bool())
    val out = Decoupled(UInt(8.W))
    val recordDone = Output(Bool())
    val recordDropped = Output(Bool())
  })

  val buffer = Module(new InputBuffer)
  val reorder = Module(new ReorderWriter)
  // One raw channel: the loader only fetches here. Routing by type is done by
  // the AttributeRouter and the reorder entries come from the ReorderWriter.
  val loader = Module(
    new AttributeLoader(JsonPathConfig(Seq(FieldSpec(Seq("unused")))))
  )
  val unpacker = Module(new WordUnpacker)
  val router = Module(new AttributeRouter(layout))
  val numberParser = Module(new NumberParser)
  val numbers = Module(new NumberToValue(layout))
  val stage = Module(new OutputStage(layout))

  buffer.io.writeEnable := io.writeEnable
  buffer.io.writeAddress := io.writeAddress
  buffer.io.writeData := io.writeData
  buffer.io.readEnable := loader.io.readEnable
  buffer.io.readAddress := loader.io.readAddress
  loader.io.readData := buffer.io.readData
  loader.io.readValid := buffer.io.readValid

  reorder.io.in <> io.descriptor
  reorder.io.endOfRecord := io.endOfRecord
  loader.io.descriptor <> reorder.io.out
  stage.io.reorder <> reorder.io.reorder

  unpacker.io.in <> loader.io.channels(0)
  loader.io.reorder.ready := true.B
  loader.io.endOfRecord := false.B // the ReorderWriter writes the end of record
  router.io.in <> unpacker.io.out
  numberParser.io.in <> router.io.numbers
  numbers.io.in <> numberParser.io.out
  stage.io.ints <> numbers.io.ints
  stage.io.floats <> numbers.io.floats

  stringParser match {
    case Some(build) =>
      val (in, out, last) = build()
      val beats = Module(new StringFrameToBeats)
      in <> router.io.strings
      beats.io.in <> out
      beats.io.inLast := last
      stage.io.strings <> beats.io.out
    case None =>
      router.io.strings.ready := true.B
      stage.io.strings.valid := false.B
      stage.io.strings.bits := DontCare
  }

  io.out <> stage.io.out
  io.recordDone := stage.io.recordDone
  io.recordDropped := stage.io.recordDropped
}

object ExecutionHarnessDriver {

  /** Where the PDA would project `"key":value`: from the colon up to the next
    * ',' or '}' outside a string.
    */
  def project(json: String, key: String): (Int, Int) = {
    val colon = json.indexOf("\"" + key + "\":") + key.length + 2
    var end = colon + 1
    var inString = false
    while (inString || (json(end) != ',' && json(end) != '}')) {
      if (json(end) == '"' && json(end - 1) != '\\') inString = !inString
      end += 1
    }
    (colon, end - colon)
  }

  /** Writes the record, plays the PDA, and returns the output bytes and the
    * number of dropped records.
    */
  def playRecord(
      c: ExecutionHarness,
      json: String,
      attributes: Seq[(Int, String)]
  ): (Seq[Int], Int) = {
    val output = mutable.ArrayBuffer.empty[Int]
    var dropped = 0
    var done = false
    def step(): Unit = {
      if (c.io.out.valid.peek().litToBoolean) {
        output += c.io.out.bits.peek().litValue.toInt
      }
      if (c.io.recordDropped.peek().litToBoolean) dropped += 1
      if (c.io.recordDone.peek().litToBoolean) done = true
      c.clock.step()
    }

    c.io.out.ready.poke(true.B)
    c.io.descriptor.valid.poke(false.B)
    c.io.endOfRecord.poke(false.B)
    for ((word, address) <- json.getBytes("UTF-8").grouped(8).zipWithIndex) {
      val data = word.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (b, i)) =>
        acc | (BigInt(b & 0xff) << (8 * i))
      }
      c.io.writeEnable.poke(true.B)
      c.io.writeAddress.poke(address.U)
      c.io.writeData.poke(data.U)
      c.clock.step()
    }
    c.io.writeEnable.poke(false.B)

    // Like the PDA, pulse endOfRecord in the cycle of the last descriptor.
    for (((id, key), i) <- attributes.zipWithIndex) {
      val (start, length) = project(json, key)
      c.io.descriptor.valid.poke(true.B)
      c.io.descriptor.bits.attributeId.poke(id.U)
      c.io.descriptor.bits.startAddress.poke(start.U)
      c.io.descriptor.bits.length.poke(length.U)
      var waited = 0
      while (!c.io.descriptor.ready.peek().litToBoolean) {
        assert(waited < 2000, "descriptor was never accepted")
        step()
        waited += 1
      }
      c.io.endOfRecord.poke((i == attributes.length - 1).B)
      step()
      c.io.endOfRecord.poke(false.B)
      c.io.descriptor.valid.poke(false.B)
    }
    if (attributes.isEmpty) {
      c.io.endOfRecord.poke(true.B)
      step()
      c.io.endOfRecord.poke(false.B)
    }

    var waited = 0
    while (!done && waited < 5000) { step(); waited += 1 }
    assert(done, "record never reached the output stage")
    for (_ <- 0 until 100) step()
    (output.toSeq, dropped)
  }

  def le(value: BigInt, bytes: Int): Seq[Int] =
    (0 until bytes).map(i => ((value >> (8 * i)) & 0xff).toInt)
  def text(value: String, bytes: Int): Seq[Int] =
    value.getBytes("UTF-8").map(_ & 0xff).toSeq.padTo(bytes, 0)
}

class OutputBridgeSpec extends AnyFlatSpec with ChiselScalatestTester {
  import ExecutionHarnessDriver._

  "The execution stage" should "deliver AttributeLoader and NumberParser results to the OutputStage" in {
    val layout = OutputLayout(
      Seq(
        IntAttr(32), // 0 id
        IntAttr(64), // 1 user.n
        FloatAttr(32), // 2 x: float conversion is not built yet
        IntAttr(32), // 3 big: does not fit int(32)
        IntAttr(64), // 4 frac: not an integer
        IntAttr(64) // 5 word: not a number
      )
    )
    val json =
      """{"id":42,"user":{"n":-7},"x":1.5,"big":99999999999,"frac":2.5,"word":"hi"}"""
    test(new ExecutionHarness(layout)) { c =>
      // Document order, plus id 9, which is not in the layout.
      val attributes =
        Seq(
          0 -> "id",
          1 -> "n",
          2 -> "x",
          3 -> "big",
          9 -> "frac",
          4 -> "frac",
          5 -> "word"
        )
      val (output, dropped) = playRecord(c, json, attributes)
      val expected = Seq(0x03) ++ le(42, 4) ++ le(BigInt(-7), 8) ++
        Seq.fill(4 + 4 + 8 + 8)(0)
      assert(output == expected, s"Unexpected output: $output")
      assert(dropped == 0)
    }
  }

  it should "reorder attributes by id and apply the filter" in {
    val layout = OutputLayout(
      Seq(IntAttr(32), IntAttr(32)),
      Seq(Comparison(0, CompareOp.Eq, IntLit(42)))
    )
    test(new ExecutionHarness(layout)) { c =>
      val (kept, _) =
        playRecord(c, """{"b":-1,"a":42}""", Seq(1 -> "b", 0 -> "a"))
      assert(kept == Seq(0x03) ++ le(42, 4) ++ le(BigInt(-1), 4))
      val (gone, dropped) =
        playRecord(c, """{"b":-1,"a":41}""", Seq(1 -> "b", 0 -> "a"))
      assert(gone.isEmpty && dropped == 1)
      val (missing, droppedAgain) = playRecord(c, """{"b":5}""", Seq(1 -> "b"))
      assert(missing.isEmpty && droppedAgain == 1)
    }
  }

  it should "only output records whose number is greater than 5" in {
    // $[?(temperature > 5)].(temperature::int(32) | humidity::int(32))
    val layout = OutputLayout(
      Seq(IntAttr(32), IntAttr(32)),
      Seq(Comparison(0, CompareOp.Gt, IntLit(5)))
    )
    test(new ExecutionHarness(layout)) { c =>
      def record(temperature: String): (Seq[Int], Int) = playRecord(
        c,
        s"""{"temperature":$temperature,"humidity":60}""",
        Seq(0 -> "temperature", 1 -> "humidity")
      )
      def frame(temperature: BigInt): Seq[Int] =
        Seq(0x03) ++ le(temperature, 4) ++ le(60, 4)

      for (kept <- Seq(BigInt(6), BigInt(100), BigInt(Int.MaxValue))) {
        val (output, dropped) = record(kept.toString)
        assert(output == frame(kept), s"$kept > 5 was not output: $output")
        assert(dropped == 0)
      }
      // 5 is not greater than 5; 5.5 is not an int, and "warm" is not a
      // number, so both are absent, and a comparison on an absent value fails.
      for (value <- Seq("5", "3", "0", "-10", "5.5", "\"warm\"")) {
        val (output, dropped) = record(value)
        assert(output.isEmpty, s"$value should have been filtered: $output")
        assert(dropped == 1)
      }
    }
  }

  "StringFrameToBeats" should "turn StringParser frames into beats, under backpressure" in {
    test(new StringFrameToBeats) { c =>
      // Frame: id, bytes, 0x00, flags.
      def frame(id: Int, s: String, flags: Int): Seq[(Int, Boolean)] =
        (Seq(id) ++ s.map(_.toInt) ++ Seq(0)).map(_ -> false) :+ (flags -> true)
      val input = mutable.Queue.empty[(Int, Boolean)]
      input ++= frame(3, "john", 0) ++ frame(3, "", 0) ++ frame(4, "a", 1) ++
        frame(5, "xy", 2)
      val beats = mutable.ArrayBuffer.empty[(Int, Boolean, Boolean, Boolean)]
      var cycle = 0
      while ((input.nonEmpty || cycle < 10) && cycle < 500) {
        val head = input.headOption
        c.io.in.valid.poke(head.nonEmpty.B)
        head.foreach { case (b, last) =>
          c.io.in.bits.poke(b.U)
          c.io.inLast.poke(last.B)
        }
        val ready = cycle % 3 != 1
        c.io.out.ready.poke(ready.B)
        if (ready && c.io.out.valid.peek().litToBoolean) {
          val o = c.io.out.bits
          beats += ((
            o.data.peek().litValue.toInt,
            o.empty.peek().litToBoolean,
            o.last.peek().litToBoolean,
            o.ok.peek().litToBoolean
          ))
        }
        if (head.nonEmpty && c.io.in.ready.peek().litToBoolean) input.dequeue()
        c.clock.step()
        cycle += 1
      }
      // Data is only meaningful when empty is false.
      val seen = beats.map { case (d, e, l, ok) =>
        (if (e) -1 else d, l, if (l) ok else true)
      }
      val expected = "joh".map(ch => (ch.toInt, false, true)) ++
        Seq(
          ('n'.toInt, true, true),
          (-1, true, true),
          ('a'.toInt, true, false)
        ) ++
        Seq(('x'.toInt, false, true), ('y'.toInt, true, false))
      assert(seen.toSeq == expected.toSeq, s"Unexpected beats: $seen")
    }
  }
}
