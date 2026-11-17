import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AttributeLoaderHarness extends Module {
  val io = IO(new Bundle {
    val writeEnable = Input(Bool())
    val writeAddress = Input(UInt(9.W))
    val writeData = Input(UInt(64.W))
    val descriptor = Flipped(Decoupled(new AttributeDescriptor))
    val out = Decoupled(new AttributeByte)
    val readEnable = Output(Bool())
  })
  val buffer = Module(new InputBuffer)
  val loader = Module(new AttributeLoader)
  buffer.io.writeEnable := io.writeEnable
  buffer.io.writeAddress := io.writeAddress
  buffer.io.writeData := io.writeData
  buffer.io.readEnable := loader.io.readEnable
  buffer.io.readAddress := loader.io.readAddress
  loader.io.readData := buffer.io.readData
  loader.io.readValid := buffer.io.readValid
  loader.io.descriptor <> io.descriptor
  io.out <> loader.io.out
  io.readEnable := loader.io.readEnable
}

class AttributeLoaderSpec extends AnyFlatSpec with ChiselScalatestTester {
  def initialize(c: AttributeLoaderHarness): Unit = {
    c.io.writeEnable.poke(false.B)
    c.io.writeAddress.poke(0.U)
    c.io.writeData.poke(0.U)
    c.io.descriptor.valid.poke(false.B)
    c.io.descriptor.bits.attributeId.poke(0.U)
    c.io.descriptor.bits.startAddress.poke(0.U)
    c.io.descriptor.bits.length.poke(0.U)
    c.io.out.ready.poke(false.B)
  }

  def writeWord(c: AttributeLoaderHarness, address: Int, bytes: Seq[Int]): Unit = {
    val word = bytes.zipWithIndex.foldLeft(BigInt(0)) {
      case (acc, (byte, offset)) => acc | (BigInt(byte) << (8 * offset))
    }
    c.io.writeEnable.poke(true.B)
    c.io.writeAddress.poke(address.U)
    c.io.writeData.poke(word.U)
    c.clock.step()
    c.io.writeEnable.poke(false.B)
  }

  def submit(c: AttributeLoaderHarness, start: Int, length: Int, id: Int): Unit = {
    c.io.descriptor.ready.expect(true.B)
    c.io.descriptor.bits.startAddress.poke(start.U)
    c.io.descriptor.bits.length.poke(length.U)
    c.io.descriptor.bits.attributeId.poke(id.U)
    c.io.descriptor.valid.poke(true.B)
    c.clock.step()
    c.io.descriptor.valid.poke(false.B)
  }

  def collect(c: AttributeLoaderHarness, expected: Seq[Int], id: Int,
              stall: Boolean = false): Unit = {
    for ((byte, index) <- expected.zipWithIndex) {
      var cycles = 0
      while (!c.io.out.valid.peek().litToBoolean && cycles < 20) {
        c.clock.step()
        cycles += 1
      }
      c.io.out.valid.expect(true.B)
      c.io.descriptor.ready.expect(false.B)
      c.io.out.bits.data.expect(byte.U)
      c.io.out.bits.attributeId.expect(id.U)
      c.io.out.bits.last.expect((index == expected.size - 1).B)
      if (stall) {
        c.io.out.ready.poke(false.B)
        for (_ <- 0 until 3) {
          c.clock.step()
          c.io.out.valid.expect(true.B)
          c.io.out.bits.data.expect(byte.U)
          c.io.out.bits.attributeId.expect(id.U)
          c.io.out.bits.last.expect((index == expected.size - 1).B)
        }
      }
      c.io.out.ready.poke(true.B)
      c.clock.step()
      c.io.out.ready.poke(false.B)
    }
    c.io.out.valid.expect(false.B)
    c.io.descriptor.ready.expect(true.B)
  }

  "AttributeLoader" should "extract selected bytes within a word" in {
    test(new AttributeLoaderHarness) { c =>
      initialize(c)
      writeWord(c, 0, 0 until 8)
      submit(c, 2, 4, 7)
      collect(c, Seq(2, 3, 4, 5), 7)
    }
  }

  it should "cross word boundaries" in {
    test(new AttributeLoaderHarness) { c =>
      initialize(c)
      for (row <- 0 until 3) writeWord(c, row, row * 8 until row * 8 + 8)
      submit(c, 6, 12, 1)
      collect(c, 6 until 18, 1)
    }
  }

  it should "accept another descriptor after finishing an attribute" in {
    test(new AttributeLoaderHarness) { c =>
      initialize(c)
      writeWord(c, 0, 0 until 8)
      submit(c, 2, 1, 1)
      collect(c, Seq(2), 1)
      submit(c, 0, 1, 2)
      collect(c, Seq(0), 2)
    }
  }

  it should "hold bytes and metadata stable under backpressure" in {
    test(new AttributeLoaderHarness) { c =>
      initialize(c)
      writeWord(c, 0, 0 until 8)
      writeWord(c, 1, 8 until 16)
      submit(c, 7, 3, 255)
      collect(c, Seq(7, 8, 9), 255, stall = true)
    }
  }

  it should "finish at the final buffer byte without wrapping" in {
    test(new AttributeLoaderHarness) { c =>
      initialize(c)
      writeWord(c, 511, 240 until 248)
      submit(c, 4094, 2, 3)
      collect(c, Seq(246, 247), 3)
      c.io.readEnable.expect(false.B)
    }
  }

}
