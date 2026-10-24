import chisel3._
import chisel3.util.{Decoupled, is, switch}

object TxState extends ChiselEnum {
  val idle, transmitStart, transmitData, transmitStop = Value
}

/** Transmits bytes as 8N1 frames using ready/valid handshaking. */
class UartTx(clockHz: Int, baudRate: Int) extends Module {
  require(clockHz > 0 && baudRate > 0)
  val clocksPerBit = (clockHz.toLong + baudRate / 2) / baudRate

  val io = IO(new Bundle {
    val tx = Output(Bool())
    val in = Flipped(Decoupled(UInt(8.W)))
  })

  val bigCounter = Module(new Counter(clocksPerBit.toInt))
  bigCounter.io.start := false.B

  val stateReg = RegInit(TxState.idle)
  val buffer = RegInit(0.U(8.W))
  val transmittedBits = RegInit(0.U(3.W))
  io.in.ready := false.B
  io.tx := true.B

  switch(stateReg) {
    is(TxState.idle) {
      io.in.ready := true.B
      transmittedBits := 0.U
      when(io.in.fire) {
        buffer := io.in.bits
        stateReg := TxState.transmitStart
        bigCounter.io.start := true.B
      }
    }
    is(TxState.transmitStart) {
      io.tx := false.B
      when(bigCounter.io.fire) {
        bigCounter.io.start := true.B
        stateReg := TxState.transmitData
      }
    }
    is(TxState.transmitData) {
      io.tx := buffer(transmittedBits)
      when(bigCounter.io.fire) {
        bigCounter.io.start := true.B
        transmittedBits := transmittedBits + 1.U
        when(transmittedBits === 7.U) {
          stateReg := TxState.transmitStop
        }
      }
    }
    is(TxState.transmitStop) {
      io.in.ready := bigCounter.io.fire
      when(bigCounter.io.fire) {
        when(io.in.fire) {
          buffer := io.in.bits
          transmittedBits := 0.U
          stateReg := TxState.transmitStart
          bigCounter.io.start := true.B
        }.otherwise {
          stateReg := TxState.idle
        }
      }
    }
  }
}
