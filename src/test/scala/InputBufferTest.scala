import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class InputBufferSpec extends AnyFlatSpec with ChiselScalatestTester {
  "InputBuffer" should "store 64-bit words and return reads one cycle later" in {
    test(new InputBuffer) { c =>
      c.io.writeEnable.poke(false.B)
      c.io.writeAddress.poke(0.U)
      c.io.writeData.poke(0.U)
      c.io.readEnable.poke(false.B)
      c.io.readAddress.poke(0.U)
      c.io.readValid.expect(false.B)

      val words = Seq(
        0 -> BigInt("0807060504030201", 16),
        1 -> BigInt("ffffffffffffffff", 16),
        511 -> BigInt("fedcba9876543210", 16)
      )

      c.io.writeEnable.poke(true.B)
      for ((address, data) <- words) {
        c.io.writeAddress.poke(address.U)
        c.io.writeData.poke(data.U)
        c.clock.step()
      }
      c.io.writeEnable.poke(false.B)

      // Consecutive requests produce consecutive responses.
      c.io.readEnable.poke(true.B)
      for ((address, data) <- words) {
        c.io.readAddress.poke(address.U)
        c.clock.step()
        c.io.readValid.expect(true.B)
        c.io.readData.expect(data.U)
      }

      c.io.readEnable.poke(false.B)
      c.clock.step()
      c.io.readValid.expect(false.B)
    }
  }

  it should "preserve an existing word when writeEnable is false" in {
    test(new InputBuffer) { c =>
      val original = BigInt("0807060504030201", 16)
      c.io.readEnable.poke(false.B)
      c.io.readAddress.poke(0.U)
      c.io.writeAddress.poke(0.U)
      c.io.writeData.poke(original.U)
      c.io.writeEnable.poke(true.B)
      c.clock.step()

      // A disabled write must leave the existing word unchanged.
      c.io.writeEnable.poke(false.B)
      c.io.writeData.poke(0.U)
      c.clock.step()
      c.io.readAddress.poke(0.U)
      c.io.readEnable.poke(true.B)
      c.clock.step()
      c.io.readValid.expect(true.B)
      c.io.readData.expect(original.U)
    }
  }
}
