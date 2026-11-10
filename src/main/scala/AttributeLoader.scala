import chisel3._
import chisel3.util._

/** Addresses and lengths are measured in bytes. Quotes, if present, are data. */
class AttributeDescriptor extends Bundle {
  val attributeId = UInt(8.W)
  val startAddress = UInt(12.W)
  val length = UInt(13.W) // Can represent the entire 4096-byte buffer.
}

class AttributeByte extends Bundle {
  val data = UInt(8.W)
  val attributeId = UInt(8.W)
  val last = Bool()
}

/** Fetches one attribute at a time from a 512 x 64-bit InputBuffer.
  * Byte offset 0 is bits 7:0. Memory responses arrive one cycle after requests.
  * Descriptors must have a nonzero length and stay within the 4096-byte buffer.
  * The caller must keep the selected memory contents unchanged until completion.
  */
class AttributeLoader extends Module {
  val io = IO(new Bundle {
    val descriptor = Flipped(Decoupled(new AttributeDescriptor))
    val out = Decoupled(new AttributeByte)

    val readEnable = Output(Bool())
    val readAddress = Output(UInt(9.W))
    val readData = Input(UInt(64.W))
    val readValid = Input(Bool())
  })

  val idle :: request :: waitForWord :: emit :: Nil = Enum(4)
  val state = RegInit(idle)
  val address = RegInit(0.U(12.W))
  val remaining = RegInit(0.U(13.W))
  val attributeId = RegInit(0.U(8.W))
  val word = RegInit(0.U(64.W))

  io.descriptor.ready := state === idle

  io.readEnable := state === request
  io.readAddress := address(11, 3)
  io.out.valid := state === emit
  io.out.bits.data := (word >> (address(2, 0) << 3))(7, 0)
  io.out.bits.attributeId := attributeId
  io.out.bits.last := remaining === 1.U

  switch(state) {
    is(idle) {
      when(io.descriptor.fire) {
        address := io.descriptor.bits.startAddress
        remaining := io.descriptor.bits.length
        attributeId := io.descriptor.bits.attributeId
        state := request
      }
    }
    is(request) {
      state := waitForWord
    }
    is(waitForWord) {
      when(io.readValid) {
        word := io.readData
        state := emit
      }
    }
    is(emit) {
      when(io.out.fire) {
        when(remaining === 1.U) {
          state := idle
        }.otherwise {
          address := address + 1.U
          remaining := remaining - 1.U
          when(address(2, 0) === 7.U) {
            state := request
          }
        }
      }
    }
  }
}
