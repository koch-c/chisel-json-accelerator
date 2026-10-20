import chisel3._
import chisel3.util.Decoupled
import chisel3.util.switch
import chisel3.util.is
// Chisel hardware, not ordinary Scala:
// - Use := for hardware connections; === for hardware equality.
// - Use when/.otherwise for hardware conditions.
// - Give UInt literals explicit widths where appropriate.
// - Follow the existing ready/valid handshake conventions.

// define chisel states states
object RxState extends ChiselEnum {
  val idle, checkStart, receiveData, checkStop = Value
}

class UartRx(clockHz: Int, baudRate: Int) extends Module {
  val io = IO(new Bundle {
    val rx = Input(Bool())
    val out = Decoupled(UInt(8.W))
  })

  val clockPerBaud = clockHz / baudRate
  val smallCounter = Module(new Counter(clockPerBaud / 2))
  smallCounter.io.tick := false.B
  val bigCounter = Module(new Counter(clockPerBaud))
  bigCounter.io.tick := false.B

  val stateReg = RegInit(RxState.idle)

  val rx_safe = RegNext(RegNext(io.rx, true.B), true.B)
  rx_safe := Reg(io.rx)
  val out_reg = RegInit(VecInit(Seq.fill(8)(false.B)))
  val receivedBits = RegInit(0.U(4.W))

  io.out.bits := out_reg.asUInt
  io.out.valid := false.B

  switch(stateReg) {
    is(RxState.idle) {
      when(!rx_safe) {
        stateReg := RxState.checkStart
        smallCounter.io.tick := true.B
      }
    }
    is(RxState.checkStart) {
      when(smallCounter.io.fire) {
        when(!rx_safe) {
          stateReg := RxState.receiveData
          bigCounter.io.tick := true.B
        }.otherwise {
          stateReg := RxState.idle
        }
      }.otherwise {
        smallCounter.io.tick := true.B
      }
    }
    is(RxState.receiveData) {
      when(bigCounter.io.fire) {
        receivedBits := receivedBits + 1.U
        when(receivedBits <= 7.U) {
          out_reg(receivedBits) := rx_safe
          bigCounter.io.tick := true.B
        }
        when(receivedBits === 8.U) {
          receivedBits := 0.U
          stateReg := RxState.checkStop
          smallCounter.io.tick := true.B
        }.otherwise {
          bigCounter.io.tick := true.B
        }
      }.otherwise {
        bigCounter.io.tick := true.B
      }
    }
    is(RxState.checkStop) {
      when(smallCounter.io.fire) {
        when(rx_safe) {
          io.out.valid := true.B
        }
        stateReg := RxState.idle
      }.otherwise {
        smallCounter.io.tick := true.B
      }
    }
  }

}
