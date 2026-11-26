import chisel3._
import chisel3.util._

/** The execution stage consumes tagged words and reorder entries from the
  * AttributeLoader. Its parser channels are SPEAR-JSON Fig. 2, block 4.
  *
  * For every value type in use there are config.parsersPerType channels. The
  * loader deals the attributes of a type to its channels in rotation.
  *
  * Number channels work in parallel: each parses its attribute and holds the
  * finished frame until it is its turn to send. String and Raw channels stream
  * their output while they read, so they only make progress while they are
  * sending and extra channels of those types give no speedup yet.
  *
  * The output is in document order. The loader writes the attribute id of every
  * attribute to the reorder FIFO. The output side reads the ids one at a time
  * and follows the same rotation as the loader to find the channel that holds
  * the attribute. That channel keeps the output until its final byte is
  * accepted, so frames are never mixed.
  */
class ExecutionStage(config: JsonPathConfig, reorderDepth: Int = 16)
    extends Module {
  val io = IO(new Bundle {
    val channels =
      Flipped(Vec(config.channelTypes.length, Decoupled(new AttributeWord)))
    val reorder = Flipped(Decoupled(new ReorderEntry))
    val out = Decoupled(UInt(8.W))
    val recordDrained = Output(Bool()) // pulses when the end marker is popped
  })

  private val perType = config.parsersPerType

  val channels = config.channelTypes.map(t => Module(new ParserChannel(t)))
  val reorder = Module(new Queue(new ReorderEntry, reorderDepth))

  for ((channel, i) <- channels.zipWithIndex) {
    channel.io.in <> io.channels(i)
  }
  reorder.io.enq <> io.reorder

  // ---- Output in document order -----------------------------------------
  // The same rotation as in the loader: one pointer per value type.
  val pointers = RegInit(
    VecInit(
      Seq.fill(config.usedValueTypes.length)(0.U(log2Ceil(perType max 2).W))
    )
  )
  val headId = reorder.io.deq.bits.attributeId
  val headValid = reorder.io.deq.valid
  // An end marker belongs to no channel. Pop it as soon as it reaches the head.
  val headIsEnd = headValid && reorder.io.deq.bits.endOfRecord
  val isType = config.usedValueTypes.map { valueType =>
    config.fields.zipWithIndex
      .collect {
        case (field, i) if field.valueType == valueType => headId === i.U
      }
      .reduce(_ || _)
  }
  val head = Mux1H(
    isType,
    pointers.zipWithIndex.map { case (pointer, k) =>
      (k * perType).U +& pointer
    }
  )

  // isHead(i): channel i holds the attribute that is next in document order.
  val isHead =
    channels.indices.map(i => headValid && !headIsEnd && head === i.U)

  io.out.valid := Mux1H(isHead, channels.map(_.io.out.valid))
  io.out.bits := Mux1H(isHead, channels.map(_.io.out.bits))
  for ((channel, i) <- channels.zipWithIndex) {
    channel.io.out.ready := isHead(i) && io.out.ready
  }

  // The final byte lets the next attribute send.
  val finished = io.out.fire && Mux1H(isHead, channels.map(_.io.last))
  reorder.io.deq.ready := finished || headIsEnd
  when(finished) {
    for ((pointer, k) <- pointers.zipWithIndex) {
      when(isType(k)) {
        pointer := Mux(pointer === (perType - 1).U, 0.U, pointer + 1.U)
      }
    }
  }

  io.recordDrained := reorder.io.deq.fire && headIsEnd
}
