import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/** An ExecutionStage with the InputBuffer it reads from. */
class ExecutionStageHarness(config: JsonPathConfig) extends Module {
  val io = IO(new Bundle {
    val writeEnable = Input(Bool())
    val writeAddress = Input(UInt(9.W))
    val writeData = Input(UInt(64.W))
    val descriptor = Flipped(Decoupled(new AttributeDescriptor))
    val endOfRecord = Input(Bool())
    val out = Decoupled(UInt(8.W))
    val busy = Output(Bool())
  })
  val buffer = Module(new InputBuffer)
  val stage = Module(new ExecutionStage(config))
  buffer.io.writeEnable := io.writeEnable
  buffer.io.writeAddress := io.writeAddress
  buffer.io.writeData := io.writeData
  buffer.io.readEnable := stage.io.readEnable
  buffer.io.readAddress := stage.io.readAddress
  stage.io.readData := buffer.io.readData
  stage.io.readValid := buffer.io.readValid
  stage.io.descriptor <> io.descriptor
  stage.io.endOfRecord := io.endOfRecord
  io.out <> stage.io.out
  io.busy := stage.io.busy
}

class ExecutionStageSpec extends AnyFlatSpec with ChiselScalatestTester {

  /** Drives the harness. Attributes are written to the buffer one after the
    * other and submitted as descriptors; the output bytes are collected. With
    * stall, output is only accepted every third cycle.
    */
  private class Driver(c: ExecutionStageHarness, stall: Boolean) {
    val output = scala.collection.mutable.ArrayBuffer.empty[Int]
    var allowOutput = true
    private var cycles = 0

    c.io.writeEnable.poke(false.B)
    c.io.descriptor.valid.poke(false.B)
    c.io.endOfRecord.poke(false.B)
    c.io.out.ready.poke(false.B)

    def tick(): Unit = {
      val ready = allowOutput && (!stall || cycles % 3 == 0)
      c.io.out.ready.poke(ready.B)
      if (ready && c.io.out.valid.peek().litToBoolean) {
        output += c.io.out.bits.peek().litValue.toInt
      }
      c.clock.step()
      cycles += 1
    }

    /** Writes the attributes back to back from address 0 and returns the start
      * address of each.
      */
    def load(attributes: Seq[String]): Seq[Int] = {
      val bytes = attributes.mkString.getBytes("UTF-8").map(_ & 0xff)
      for ((word, address) <- bytes.grouped(8).zipWithIndex) {
        val data = word.zipWithIndex.foldLeft(BigInt(0)) {
          case (acc, (byte, offset)) => acc | (BigInt(byte) << (8 * offset))
        }
        c.io.writeEnable.poke(true.B)
        c.io.writeAddress.poke(address.U)
        c.io.writeData.poke(data.U)
        tick()
      }
      c.io.writeEnable.poke(false.B)
      attributes.scanLeft(0)(_ + _.getBytes("UTF-8").length).init
    }

    /** Offers one descriptor until it is accepted. Returns false if it was not
      * accepted within the given number of cycles.
      */
    def submit(
        start: Int,
        length: Int,
        id: Int,
        patience: Int = 2000
    ): Boolean = {
      c.io.descriptor.bits.startAddress.poke(start.U)
      c.io.descriptor.bits.length.poke(length.U)
      c.io.descriptor.bits.attributeId.poke(id.U)
      c.io.descriptor.valid.poke(true.B)
      var waited = 0
      while (!c.io.descriptor.ready.peek().litToBoolean && waited < patience) {
        tick()
        waited += 1
      }
      val accepted = c.io.descriptor.ready.peek().litToBoolean
      if (accepted) tick()
      c.io.descriptor.valid.poke(false.B)
      accepted
    }

    /** Pulses endOfRecord for one cycle, as the PDA does. */
    def endOfRecord(): Unit = {
      c.io.endOfRecord.poke(true.B)
      tick()
      c.io.endOfRecord.poke(false.B)
    }

    /** Loads the attributes and submits one descriptor for each. */
    def run(attributes: Seq[(String, Int)]): Unit = {
      val starts = load(attributes.map(_._1))
      for (((text, id), start) <- attributes.zip(starts)) {
        assert(
          submit(start, text.getBytes("UTF-8").length, id),
          s"Descriptor for '$text' was never accepted"
        )
      }
    }

    def waitUntilIdle(): Unit = {
      var waited = 0
      while (c.io.busy.peek().litToBoolean && waited < 5000) {
        tick()
        waited += 1
      }
      assert(waited < 5000, "The execution stage stayed busy")
      for (_ <- 0 until 4) tick()
    }
  }

  private def run(config: JsonPathConfig, stall: Boolean = false)(
      body: Driver => Unit
  ): Unit =
    test(new ExecutionStageHarness(config))(c => body(new Driver(c, stall)))

  private def bytes(text: String): Seq[Int] =
    text.getBytes("UTF-8").map(_ & 0xff).toSeq

  private def stringFrame(id: Int, text: String, flags: Int = 0): Seq[Int] =
    Seq(id) ++ bytes(text) ++ Seq(0, flags)

  private def int32Frame(id: Int, value: Int): Seq[Int] =
    Seq(id, if (value < 0) 1 else 0) ++
      (0 until 4).map(i => (value >> (8 * i)) & 0xff)

  behavior of "ExecutionStage"

  it should "be idle after reset" in {
    run(JsonPathConfig(Seq(FieldSpec(Seq("a"))))) { d =>
      d.waitUntilIdle()
      assert(d.output.isEmpty)
    }
  }

  it should "send raw attributes in dispatch order with one channel" in {
    run(JsonPathConfig(Seq(FieldSpec(Seq("a"))))) { d =>
      d.run(Seq(":1" -> 0, ":\"two\"" -> 0, ": 3 " -> 0))
      d.waitUntilIdle()
      assert(d.output.toSeq == bytes(":1:\"two\": 3 "))
    }
  }

  it should "route each attribute to the parser of its path" in {
    // Field 0 is the Int32 and field 1 the string.
    val config = JsonPathConfig(
      Seq(
        FieldSpec(Seq("id"), ValueType.Int32),
        FieldSpec(Seq("user", "name"), ValueType.String)
      )
    )
    run(config) { d =>
      d.run(Seq(": 25" -> 0, ":\"Ada\"" -> 1, ":-7" -> 0, ":\"\"" -> 1))
      d.waitUntilIdle()
      assert(
        d.output.toSeq ==
          int32Frame(0, 25) ++ stringFrame(1, "Ada") ++
          int32Frame(0, -7) ++ stringFrame(1, "")
      )
    }
  }

  it should "keep the order when a later attribute finishes first" in {
    val config = JsonPathConfig(
      Seq(FieldSpec(Seq("v"), ValueType.String)),
      parsersPerType = 2
    )
    val long = "the first value is much longer than the second one"
    run(config, stall = true) { d =>
      d.run(Seq(s":\"$long\"" -> 0, ":\"b\"" -> 0, ":\"c\"" -> 0))
      d.waitUntilIdle()
      assert(
        d.output.toSeq ==
          stringFrame(0, long) ++ stringFrame(0, "b") ++ stringFrame(0, "c")
      )
    }
  }

  it should "queue more attributes than it has channels while the output is blocked" in {
    val config = JsonPathConfig(
      Seq(FieldSpec(Seq("v"), ValueType.Int32)),
      parsersPerType = 2
    )
    run(config) { d =>
      d.allowOutput = false
      val starts = d.load(Seq(": 1", ": 2", ": 3"))
      // The third attribute waits in the FIFO of the first channel.
      for (start <- starts) assert(d.submit(start, 3, 0))
      for (_ <- 0 until 40) d.tick()
      assert(d.output.isEmpty)
      d.allowOutput = true
      d.waitUntilIdle()
      assert(
        d.output.toSeq ==
          int32Frame(0, 1) ++ int32Frame(0, 2) ++ int32Frame(0, 3)
      )
    }
  }

  it should "hold the loader back while a channel FIFO is full" in {
    val config = JsonPathConfig(
      Seq(
        FieldSpec(Seq("n"), ValueType.Int32),
        FieldSpec(Seq("s"), ValueType.String)
      )
    )
    // Longer than the four words a channel FIFO holds.
    val long =
      "a string that needs many more words than one channel FIFO can hold"
    val attributes = Seq(": 1", ":\"" + long + "\"", ": 2")
    run(config) { d =>
      d.allowOutput = false
      val starts = d.load(attributes)
      def submit(i: Int, patience: Int = 2000): Boolean =
        d.submit(
          starts(i),
          attributes(i).length,
          if (i == 1) 1 else 0,
          patience
        )
      assert(submit(0))
      assert(submit(1))
      // The loader is stuck in the middle of the string, so it takes nothing.
      assert(!submit(2, patience = 60))
      assert(d.output.isEmpty)
      d.allowOutput = true
      assert(submit(2))
      d.waitUntilIdle()
      assert(
        d.output.toSeq ==
          int32Frame(0, 1) ++ stringFrame(1, long) ++ int32Frame(0, 2)
      )
    }
  }

  it should "hold a descriptor back while the reorder FIFO is full" in {
    val config = JsonPathConfig(
      Seq(FieldSpec(Seq("v"), ValueType.Int32)),
      parsersPerType = 4
    )
    val values = 1 to 20
    val attributes = values.map(v => f":$v%2d")
    run(config) { d =>
      d.allowOutput = false
      val starts = d.load(attributes)
      // The reorder FIFO holds 16 entries and none can leave.
      for (i <- 0 until 16) assert(d.submit(starts(i), 3, 0))
      assert(!d.submit(starts(16), 3, 0, patience = 60))
      assert(d.output.isEmpty)
      d.allowOutput = true
      for (i <- 16 until 20) assert(d.submit(starts(i), 3, 0))
      d.waitUntilIdle()
      assert(d.output.toSeq == values.flatMap(v => int32Frame(0, v)))
    }
  }

  it should "pass over end-of-record entries and keep the order" in {
    val config = JsonPathConfig(
      Seq(FieldSpec(Seq("v"), ValueType.Int32)),
      parsersPerType = 2
    )
    run(config) { d =>
      val attributes = Seq(": 1", ": 2", ": 3", ": 4", ": 5")
      val starts = d.load(attributes)
      // Three records of 3, 0 and 2 attributes, without a reset in between.
      for (i <- 0 until 3) assert(d.submit(starts(i), 3, 0))
      d.endOfRecord()
      for (_ <- 0 until 3) d.tick()
      d.endOfRecord()
      for (i <- 3 until 5) assert(d.submit(starts(i), 3, 0))
      d.endOfRecord()
      d.waitUntilIdle()
      assert(d.output.toSeq == (1 to 5).flatMap(v => int32Frame(0, v)))
    }
  }

  it should "stay busy until the final byte has been accepted" in {
    run(JsonPathConfig(Seq(FieldSpec(Seq("a"))))) { d =>
      d.allowOutput = false
      d.run(Seq(":12" -> 0))
      for (_ <- 0 until 30) d.tick()
      assert(d.output.isEmpty)
      d.allowOutput = true
      d.waitUntilIdle()
      assert(d.output.toSeq == bytes(":12"))
    }
  }

  it should "give the same output with one channel and with four" in {
    val random = new scala.util.Random(7)
    val attributes = Seq.fill(24) {
      val spaces = " " * random.nextInt(4)
      (s":$spaces${random.nextInt(2000000) - 1000000}$spaces", 0)
    }
    def outputWith(channels: Int): Seq[Int] = {
      var result = Seq.empty[Int]
      val config = JsonPathConfig(
        Seq(FieldSpec(Seq("v"), ValueType.Number)),
        parsersPerType = channels
      )
      run(config, stall = channels > 1) { d =>
        d.run(attributes)
        d.waitUntilIdle()
        result = d.output.toSeq
      }
      result
    }
    val reference = outputWith(1)
    assert(reference.length == 24 * 12)
    assert(outputWith(4) == reference)
  }
}
