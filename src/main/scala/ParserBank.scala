import chisel3._
import chisel3.util._

/** The back end: several ParserLanes side by side.
  *
  * For every value type in use there are config.parsersPerType lanes. A
  * descriptor goes to a free lane of the type its path asks for, so several
  * attributes can be loaded and parsed at the same time. The lanes share the
  * single read port of the InputBuffer.
  *
  * The output is in dispatch order: the lane index of every accepted descriptor
  * is put in a queue, and only the lane at the head of that queue may send. It
  * keeps the output until its final byte is accepted, so frames are never mixed
  * and come out in the order of the document.
  */
class ParserBank(config: JsonPathConfig) extends Module {
  val io = IO(new Bundle {
    val descriptor = Flipped(Decoupled(new AttributeDescriptor))
    val out = Decoupled(UInt(8.W))
    val busy = Output(Bool()) // an attribute is still being loaded or sent

    // Read port of the InputBuffer.
    val readEnable = Output(Bool())
    val readAddress = Output(UInt(9.W))
    val readData = Input(UInt(64.W))
    val readValid = Input(Bool())
  })

  // The value type of every lane, e.g. Seq(Int32, Int32, String, String).
  private val laneTypes =
    config.usedValueTypes.flatMap(t => Seq.fill(config.parsersPerType)(t))
  private val laneCount = laneTypes.length
  private val laneIndexWidth = log2Ceil(laneCount max 2)

  val lanes = laneTypes.map(t => Module(new ParserLane(t)))

  // A lane holds one attribute at a time: from the descriptor until its final
  // output byte has been accepted.
  val laneBusy = RegInit(VecInit(Seq.fill(laneCount)(false.B)))

  // The lanes in the order their attributes must be sent.
  val order = Module(new Queue(UInt(laneIndexWidth.W), laneCount))

  // ---- Dispatcher ------------------------------------------------------
  // A lane is wanted when the descriptor belongs to a path of the lane's type.
  val id = io.descriptor.bits.attributeId
  val wanted = laneTypes.map { laneType =>
    config.paths.indices
      .filter(path => config.fields(path).valueType == laneType)
      .map(path => id === path.U)
      .reduce(_ || _)
  }
  val candidates = VecInit(lanes.indices.map(i => wanted(i) && !laneBusy(i)))
  val hasCandidate = candidates.asUInt.orR
  val target = PriorityEncoder(candidates)

  io.descriptor.ready := hasCandidate && order.io.enq.ready
  order.io.enq.valid := io.descriptor.valid && hasCandidate
  order.io.enq.bits := target

  for ((lane, i) <- lanes.zipWithIndex) {
    lane.io.descriptor.valid := io.descriptor.fire && target === i.U
    lane.io.descriptor.bits := io.descriptor.bits
  }
  when(io.descriptor.fire) { laneBusy(target) := true.B }

  // ---- Shared read port ------------------------------------------------
  // One lane is granted per cycle. The word arrives one cycle later, so the
  // granted lane is remembered to hand readValid to that lane only.
  val readArbiter = Module(new Arbiter(UInt(9.W), laneCount))
  for ((lane, i) <- lanes.zipWithIndex) {
    readArbiter.io.in(i) <> lane.io.read
  }
  readArbiter.io.out.ready := true.B
  io.readEnable := readArbiter.io.out.valid
  io.readAddress := readArbiter.io.out.bits

  val grantedLane = RegNext(readArbiter.io.chosen)
  for ((lane, i) <- lanes.zipWithIndex) {
    lane.io.readData := io.readData
    lane.io.readValid := io.readValid && grantedLane === i.U
  }

  // ---- Output in dispatch order ----------------------------------------
  val head = order.io.deq.bits
  val headValid = order.io.deq.valid
  val laneValid = VecInit(lanes.map(_.io.out.valid))
  val laneBits = VecInit(lanes.map(_.io.out.bits))
  val laneLast = VecInit(lanes.map(_.io.last))

  io.out.valid := headValid && laneValid(head)
  io.out.bits := laneBits(head)
  for ((lane, i) <- lanes.zipWithIndex) {
    lane.io.out.ready := headValid && head === i.U && io.out.ready
  }

  // The final byte frees the lane and lets the next one send.
  val finished = io.out.fire && laneLast(head)
  order.io.deq.ready := finished
  when(finished) { laneBusy(head) := false.B }

  io.busy := headValid
}
