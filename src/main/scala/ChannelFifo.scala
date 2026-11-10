import chisel3._
import chisel3.util._

/** A packed batch: only entries 0 until count are meaningful.
  * A valid batch must contain between one and width entries.
  */
class ChannelBatch[T <: Data](gen: T, width: Int) extends Bundle {
  require(width > 0, "Batch width must be positive")

  val entries = Vec(width, gen)
  val count = UInt(log2Ceil(width + 1).W)
}

/** Ordered, multi-entry enqueue/dequeue FIFO built from single-entry Queues.
  *
  * Depth is measured in individual entries, not batches. Each handshake
  * transfers the complete packed batch. The producer must compact sparse
  * symbols into leading lanes before submitting a batch.
  *
  * Consecutive entries are striped across max(enqueueWidth, dequeueWidth)
  * banks. Depth must be a multiple of the bank count. The registered output
  * batch counts toward depth, so total accepted-but-not-consumed entries never
  * exceed depth. Output batches may be partial; full batches are not awaited.
  *
  * Input acceptance is atomic and conservative: it uses current free capacity
  * and bank readiness, without looking ahead to simultaneous dequeues. There
  * is no empty-queue bypass. Output entries and count remain stable when stalled.
  */
class ChannelFifo[T <: Data](
    gen: T,
    depth: Int,
    enqueueWidth: Int = 8,
    dequeueWidth: Int = 2
) extends Module {
  require(enqueueWidth > 0 && dequeueWidth > 0, "Batch widths must be positive")
  require(depth >= math.max(enqueueWidth, dequeueWidth),
    "Channel FIFO must hold at least one maximum-sized input or output batch")
  private val bankCount = math.max(enqueueWidth, dequeueWidth)
  require(isPow2(bankCount), "Bank count must be a power of two")
  require(depth % bankCount == 0, "Depth must be a multiple of the bank count")

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new ChannelBatch(gen, enqueueWidth)))
    val out = Decoupled(new ChannelBatch(gen, dequeueWidth))
  })

  val banks = Seq.fill(bankCount)(Module(new Queue(gen, depth / bankCount,
    pipe = false, flow = false)))
  val pointerWidth = math.max(1, log2Ceil(bankCount))
  val writeBank = RegInit(0.U(pointerWidth.W))
  val readBank = RegInit(0.U(pointerWidth.W))
  val occupancy = RegInit(0.U(log2Ceil(depth + 1).W))

  def bankAt(pointer: UInt, offset: UInt): UInt =
    ((pointer +& offset) % bankCount.U)(pointerWidth - 1, 0)

  def selectBank[A <: Data](values: Vec[A], index: UInt): A =
    if (bankCount == 1) values(0) else values(index)

  val inputBanks = VecInit((0 until enqueueWidth).map(i => bankAt(writeBank, i.U)))
  val bankReady = VecInit(banks.map(_.io.enq.ready))
  val countValid = io.in.bits.count > 0.U && io.in.bits.count <= enqueueWidth.U
  val destinationsReady = (0 until enqueueWidth).map { i =>
    i.U >= io.in.bits.count || selectBank(bankReady, inputBanks(i))
  }.reduce(_ && _)
  io.in.ready := countValid && destinationsReady &&
    (occupancy +& io.in.bits.count) <= depth.U

  for ((bank, index) <- banks.zipWithIndex) {
    bank.io.enq.valid := false.B
    bank.io.enq.bits := 0.U.asTypeOf(gen)
    for (i <- 0 until enqueueWidth) {
      when(i.U < io.in.bits.count && inputBanks(i) === index.U) {
        bank.io.enq.valid := io.in.fire
        bank.io.enq.bits := io.in.bits.entries(i)
      }
    }
  }
  when(io.in.fire) {
    writeBank := bankAt(writeBank, io.in.bits.count)
  }
  val outputBatch = Reg(new ChannelBatch(gen, dequeueWidth))
  val outputValid = RegInit(false.B)
  io.out.valid := outputValid
  io.out.bits := outputBatch

  val outputBanks = VecInit((0 until dequeueWidth).map(i => bankAt(readBank, i.U)))
  val bankValid = VecInit(banks.map(_.io.deq.valid))
  val bankData = VecInit(banks.map(_.io.deq.bits))
  // Only take a consecutive prefix; never skip a missing entry.
  val available = VecInit((0 until dequeueWidth).map { i =>
    (0 to i).map(j => selectBank(bankValid, outputBanks(j))).reduce(_ && _)
  })
  val takeCount = PopCount(available)
  val loadOutput = (!outputValid || io.out.ready) && takeCount > 0.U

  for ((bank, index) <- banks.zipWithIndex) {
    bank.io.deq.ready := loadOutput && (0 until dequeueWidth).map { i =>
      available(i) && outputBanks(i) === index.U
    }.reduce(_ || _)
  }
  when(loadOutput) {
    for (i <- 0 until dequeueWidth) {
      outputBatch.entries(i) := Mux(available(i), selectBank(bankData, outputBanks(i)),
        0.U.asTypeOf(gen))
    }
    outputBatch.count := takeCount
    outputValid := true.B
    readBank := bankAt(readBank, takeCount)
  }.elsewhen(io.out.fire) {
    outputValid := false.B
  }

  val accepted = Mux(io.in.fire, io.in.bits.count, 0.U)
  val consumed = Mux(io.out.fire, outputBatch.count, 0.U)
  occupancy := (occupancy +& accepted) - consumed
}
