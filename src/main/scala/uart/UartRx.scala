import chisel3._
import chisel3.util.{Decoupled, is, switch}

object RxState extends ChiselEnum {
  val idle, checkStart, receiveData, checkStop = Value
}

/** Receives 8N1 frames. If the output is full, a new byte is discarded. */
class UartRx(clockHz: Int, baudRate: Int) extends Module {
  require(clockHz > 0 && baudRate > 0)
  val clocksPerBit = (clockHz.toLong + baudRate / 2) / baudRate

  val io = IO(new Bundle {
    val rx = Input(Bool())
    val out = Decoupled(UInt(8.W))
  })

  val smallCounter = Module(new Counter((clocksPerBit / 2).toInt))
  val bigCounter = Module(new Counter(clocksPerBit.toInt))
  smallCounter.io.start := false.B
  bigCounter.io.start := false.B

  val stateReg = RegInit(RxState.idle)
  val rx_safe = RegNext(RegNext(io.rx, true.B), true.B)
  val previousRx = RegNext(rx_safe, true.B)
  val falling = previousRx && !rx_safe
  val out_reg = RegInit(VecInit(Seq.fill(8)(false.B)))
  val receivedBits = RegInit(0.U(3.W))
  val outputByte = RegInit(0.U(8.W))
  val outputValid = RegInit(false.B)

  io.out.bits := outputByte
  io.out.valid := outputValid
  when(io.out.fire) {
    outputValid := false.B
  }

  switch(stateReg) {
    is(RxState.idle) {
      when(falling) {
        smallCounter.io.start := true.B
        stateReg := RxState.checkStart
      }
    }
    is(RxState.checkStart) {
      when(smallCounter.io.fire) {
        when(!rx_safe) {
          receivedBits := 0.U
          bigCounter.io.start := true.B
          stateReg := RxState.receiveData
        }.otherwise {
          stateReg := RxState.idle
        }
      }
    }
    is(RxState.receiveData) {
      when(bigCounter.io.fire) {
        out_reg(receivedBits) := rx_safe
        // Restart for the next data bit, or for the stop-bit midpoint.
        bigCounter.io.start := true.B
        when(receivedBits === 7.U) {
          stateReg := RxState.checkStop
        }.otherwise {
          receivedBits := receivedBits + 1.U
        }
      }
    }
    is(RxState.checkStop) {
      when(bigCounter.io.fire) {
        // Never overwrite a byte that the consumer has not accepted.
        when(rx_safe && (!outputValid || io.out.ready)) {
          outputByte := out_reg.asUInt
          outputValid := true.B
        }
        stateReg := RxState.idle
      }
    }
  }
}
