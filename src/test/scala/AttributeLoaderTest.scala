import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/** An AttributeLoader with the InputBuffer it reads from. */
class AttributeLoaderHarness(config: JsonPathConfig) extends Module {
  val io = IO(new Bundle {
    val writeEnable = Input(Bool())
    val writeAddress = Input(UInt(9.W))
    val writeData = Input(UInt(64.W))
    val descriptor = Flipped(Decoupled(new AttributeDescriptor))
    val channels =
      Vec(config.channelTypes.length, Decoupled(new AttributeWord))
    val reorder = Decoupled(new ReorderEntry)
  })

  val buffer = Module(new InputBuffer)
  val loader = Module(new AttributeLoader(config))

  buffer.io.writeEnable := io.writeEnable
  buffer.io.writeAddress := io.writeAddress
  buffer.io.writeData := io.writeData
  buffer.io.readEnable := loader.io.readEnable
  buffer.io.readAddress := loader.io.readAddress
  loader.io.readData := buffer.io.readData
  loader.io.readValid := buffer.io.readValid

  loader.io.descriptor <> io.descriptor
  io.channels <> loader.io.channels
  io.reorder <> loader.io.reorder
}

class AttributeLoaderSpec extends AnyFlatSpec with ChiselScalatestTester {

  /** One word as a channel received it. text holds only the tagged bytes. */
  private case class Word(
      channel: Int,
      text: String,
      firstByte: Int,
      lastByte: Int,
      last: Boolean,
      id: Int
  )

  private class Driver(c: AttributeLoaderHarness, channels: Int) {
    val words = scala.collection.mutable.ArrayBuffer.empty[Word]
    val reorderIds = scala.collection.mutable.ArrayBuffer.empty[Int]
    val channelReady = Array.fill(channels)(true)

    c.io.writeEnable.poke(false.B)
    c.io.descriptor.valid.poke(false.B)
    c.io.reorder.ready.poke(true.B)

    def tick(): Unit = {
      for (i <- 0 until channels) {
        val port = c.io.channels(i)
        port.ready.poke(channelReady(i).B)
        if (channelReady(i) && port.valid.peek().litToBoolean) {
          val data = port.bits.data.peek().litValue
          val first = port.bits.firstByte.peek().litValue.toInt
          val last = port.bits.lastByte.peek().litValue.toInt
          val text = (first to last)
            .map(lane => ((data >> (8 * lane)) & 0xff).toInt.toChar)
            .mkString
          words += Word(
            i,
            text,
            first,
            last,
            port.bits.last.peek().litToBoolean,
            port.bits.attributeId.peek().litValue.toInt
          )
        }
      }
      if (c.io.reorder.valid.peek().litToBoolean) {
        reorderIds += c.io.reorder.bits.attributeId.peek().litValue.toInt
      }
      c.clock.step()
    }

    /** Writes the text to the buffer, starting at the given word. */
    def load(text: String, firstWord: Int = 0): Unit = {
      for ((word, index) <- text.getBytes("UTF-8").grouped(8).zipWithIndex) {
        val address = firstWord + index
        val data = word.zipWithIndex.foldLeft(BigInt(0)) {
          case (acc, (byte, offset)) =>
            acc | (BigInt(byte & 0xff) << (8 * offset))
        }
        c.io.writeEnable.poke(true.B)
        c.io.writeAddress.poke(address.U)
        c.io.writeData.poke(data.U)
        tick()
      }
      c.io.writeEnable.poke(false.B)
    }

    /** The word a channel is offering right now, without taking it. */
    def offered(channel: Int): Option[Word] = {
      val port = c.io.channels(channel)
      if (!port.valid.peek().litToBoolean) None
      else {
        val data = port.bits.data.peek().litValue
        val first = port.bits.firstByte.peek().litValue.toInt
        val last = port.bits.lastByte.peek().litValue.toInt
        val text = (first to last)
          .map(lane => ((data >> (8 * lane)) & 0xff).toInt.toChar)
          .mkString
        Some(
          Word(
            channel,
            text,
            first,
            last,
            port.bits.last.peek().litToBoolean,
            port.bits.attributeId.peek().litValue.toInt
          )
        )
      }
    }

    /** Offers one descriptor until the loader takes it. */
    def submit(start: Int, length: Int, id: Int): Unit = {
      c.io.descriptor.bits.startAddress.poke(start.U)
      c.io.descriptor.bits.length.poke(length.U)
      c.io.descriptor.bits.attributeId.poke(id.U)
      c.io.descriptor.valid.poke(true.B)
      var waited = 0
      while (!c.io.descriptor.ready.peek().litToBoolean) {
        tick()
        waited += 1
        assert(waited < 200, "The descriptor was never accepted")
      }
      tick()
      c.io.descriptor.valid.poke(false.B)
    }

    def waitForWords(count: Int): Unit = {
      var waited = 0
      while (words.length < count) {
        tick()
        waited += 1
        assert(waited < 200, s"Expected $count words, got ${words.length}")
      }
    }
  }

  private def run(config: JsonPathConfig)(body: Driver => Unit): Unit =
    test(new AttributeLoaderHarness(config)) { c =>
      body(new Driver(c, config.channelTypes.length))
    }

  private val raw = JsonPathConfig(Seq(FieldSpec(Seq("a"))))
  private val text = "0123456789abcdefghijklmnopqrstuv"

  behavior of "AttributeLoader"

  it should "send an attribute inside one word as a single tagged word" in {
    run(raw) { d =>
      d.load(text)
      d.submit(start = 2, length = 4, id = 0)
      d.waitForWords(1)
      assert(d.words.toSeq == Seq(Word(0, "2345", 2, 5, last = true, 0)))
      assert(d.reorderIds.toSeq == Seq(0))
    }
  }

  it should "split an attribute that crosses a word boundary" in {
    run(raw) { d =>
      d.load(text)
      d.submit(start = 13, length = 6, id = 0)
      d.waitForWords(2)
      assert(
        d.words.toSeq == Seq(
          Word(0, "def", 5, 7, last = false, 0),
          Word(0, "ghi", 0, 2, last = true, 0)
        )
      )
    }
  }

  it should "send whole words in the middle of a long attribute" in {
    run(raw) { d =>
      d.load(text)
      d.submit(start = 6, length = 20, id = 0)
      d.waitForWords(4)
      assert(d.words.map(_.text).mkString == text.substring(6, 26))
      assert(
        d.words.map(w => (w.firstByte, w.lastByte)) ==
          Seq((6, 7), (0, 7), (0, 7), (0, 1))
      )
      assert(d.words.map(_.last) == Seq(false, false, false, true))
    }
  }

  it should "deal the attributes of one type to its channels in rotation" in {
    val config = JsonPathConfig(
      Seq(FieldSpec(Seq("v"), ValueType.Int32)),
      parsersPerType = 2
    )
    run(config) { d =>
      d.load(text)
      d.submit(0, 2, 0)
      d.submit(2, 2, 0)
      d.submit(4, 2, 0)
      d.waitForWords(3)
      assert(d.words.map(_.channel) == Seq(0, 1, 0))
      assert(d.words.map(_.text) == Seq("01", "23", "45"))
      assert(d.reorderIds.toSeq == Seq(0, 0, 0))
    }
  }

  it should "send each attribute to a channel of its value type" in {
    val config = JsonPathConfig(
      Seq(
        FieldSpec(Seq("n"), ValueType.Int32),
        FieldSpec(Seq("s"), ValueType.String)
      )
    )
    run(config) { d =>
      d.load(text)
      d.submit(0, 2, id = 1)
      d.submit(2, 2, id = 0)
      d.submit(4, 2, id = 1)
      d.waitForWords(3)
      assert(d.words.map(_.channel) == Seq(1, 0, 1))
      assert(d.words.map(_.id) == Seq(1, 0, 1))
      assert(d.reorderIds.toSeq == Seq(1, 0, 1))
    }
  }

  it should "hold a word until its channel takes it" in {
    run(raw) { d =>
      d.load(text)
      d.channelReady(0) = false
      d.submit(start = 13, length = 6, id = 0)
      for (_ <- 0 until 20) d.tick()
      assert(d.words.isEmpty)
      d.channelReady(0) = true
      d.waitForWords(2)
      assert(d.words.map(_.text).mkString == "defghi")
    }
  }

  it should "keep a stalled word and its tags unchanged" in {
    run(raw) { d =>
      d.load(text)
      d.channelReady(0) = false
      d.submit(start = 13, length = 6, id = 0)
      for (_ <- 0 until 5) d.tick()
      val first = Word(0, "def", 5, 7, last = false, 0)
      for (_ <- 0 until 10) {
        assert(d.offered(0).contains(first))
        d.tick()
      }
      d.channelReady(0) = true
      d.waitForWords(2)
      assert(d.words.toSeq == Seq(first, Word(0, "ghi", 0, 2, last = true, 0)))
    }
  }

  it should "finish at the final buffer byte and take the next descriptor" in {
    run(raw) { d =>
      d.load(text)
      d.load("ABCDEFGH", firstWord = 511)
      d.submit(start = 4094, length = 2, id = 0)
      d.waitForWords(1)
      assert(d.words.toSeq == Seq(Word(0, "GH", 6, 7, last = true, 0)))
      // The address must not wrap around into another read.
      for (_ <- 0 until 10) d.tick()
      assert(d.words.length == 1)
      d.submit(start = 0, length = 3, id = 0)
      d.waitForWords(2)
      assert(d.words.last == Word(0, "012", 0, 2, last = true, 0))
    }
  }

  it should "carry the largest attribute id to the word and the reorder entry" in {
    val config = JsonPathConfig((0 until 256).map(i => FieldSpec(Seq(s"k$i"))))
    run(config) { d =>
      d.load(text)
      d.submit(start = 2, length = 4, id = 255)
      d.waitForWords(1)
      assert(d.words.toSeq == Seq(Word(0, "2345", 2, 5, last = true, 255)))
      assert(d.reorderIds.toSeq == Seq(255))
    }
  }
}
