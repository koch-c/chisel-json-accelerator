import chisel3._

/** Stores 512 eight-byte words (4096 bytes) for the attribute loader.
  * Addresses select words, not bytes. Byte 0 occupies bits 7:0.
  * Reads return one cycle after readEnable; the consumer must accept the result.
  * Contents are not cleared on reset. Do not read unwritten locations or read
  * and write the same address in the same cycle (the result is undefined).
  */
class InputBuffer extends Module {
  val io = IO(new Bundle {
    val writeEnable = Input(Bool())
    val writeAddress = Input(UInt(9.W))
    val writeData = Input(UInt(64.W))

    val readEnable = Input(Bool())
    val readAddress = Input(UInt(9.W))
    val readData = Output(UInt(64.W))
    val readValid = Output(Bool())
  })

  val memory = SyncReadMem(512, UInt(64.W))

  when(io.writeEnable) {
    memory.write(io.writeAddress, io.writeData)
  }

  io.readData := memory.read(io.readAddress, io.readEnable)
  io.readValid := RegNext(io.readEnable, false.B)
}
