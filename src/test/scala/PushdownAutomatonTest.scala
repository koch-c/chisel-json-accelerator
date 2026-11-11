import chisel3._
import chisel3.util.Decoupled
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class PushdownAutomatonSpec extends AnyFlatSpec with ChiselScalatestTester {
  private case class TestSymbol(kind: SymbolKind.Type, address: Int, keyId: Int = 0)
  private case class Result(attributeId: BigInt, address: BigInt, length: BigInt)

  private def send(c: PushdownAutomaton, symbols: Seq[TestSymbol], config: JsonPathConfig)
      : (Seq[Result], Int) = {
    val results = scala.collection.mutable.ArrayBuffer.empty[Result]
    var endOfRecords = 0
    val keyIdWidth = math.max(1, chisel3.util.log2Ceil(config.keyIds.size))

    for (batch <- symbols.grouped(2)) {
      c.io.in.valid.poke(true.B)
      c.io.in.bits.count.poke(batch.length.U)
      for (lane <- 0 until 2) {
        if (lane < batch.length) {
          val symbol = batch(lane)
          c.io.in.bits.entries(lane).kind.poke(symbol.kind)
          c.io.in.bits.entries(lane).keyId.poke(symbol.keyId.U(keyIdWidth.W))
          c.io.in.bits.entries(lane).addr.poke(symbol.address.U)
        } else {
          c.io.in.bits.entries(lane).kind.poke(SymbolKind.Comma)
          c.io.in.bits.entries(lane).keyId.poke(0.U)
          c.io.in.bits.entries(lane).addr.poke(0.U)
        }
      }

      c.io.descriptor.ready.poke(true.B)
      c.io.in.ready.expect(true.B)
      if (c.io.descriptor.valid.peek().litToBoolean) {
        results += Result(
          c.io.descriptor.bits.attributeId.peek().litValue,
          c.io.descriptor.bits.startAddress.peek().litValue,
          c.io.descriptor.bits.length.peek().litValue
        )
      }
      if (c.io.endOfRecord.peek().litToBoolean) endOfRecords += 1
      c.clock.step()
    }

    c.io.in.valid.poke(false.B)
    (results.toSeq, endOfRecords)
  }

  private def key(config: JsonPathConfig, name: String, addr: Int): TestSymbol =
    TestSymbol(SymbolKind.Key, addr, config.keyIds(name))

  "PushdownAutomaton" should "send a loader descriptor for the requested nested path" in {
    val config = JsonPathConfig(Seq("device", "location", "latitude"), attributeId = 7)
    test(new PushdownAutomaton(config)) { c =>
      val symbols = Seq(
        TestSymbol(SymbolKind.ObjOpen, 0),
        key(config, "device", 8),
        TestSymbol(SymbolKind.Colon, 9),
        TestSymbol(SymbolKind.ObjOpen, 10),
        key(config, "location", 20),
        TestSymbol(SymbolKind.Colon, 21),
        TestSymbol(SymbolKind.ObjOpen, 22),
        key(config, "latitude", 32),
        TestSymbol(SymbolKind.Colon, 33),
        TestSymbol(SymbolKind.ObjClose, 38),
        TestSymbol(SymbolKind.ObjClose, 39),
        TestSymbol(SymbolKind.ObjClose, 40)
      )

      val (descriptors, endOfRecords) = send(c, symbols, config)
      c.io.found.expect(true.B)
      c.io.done.expect(true.B)
      c.io.depth.expect(0.U)
      assert(descriptors == Seq(Result(7, 33, 5)))
      assert(endOfRecords == 1)
    }
  }

  it should "ignore a key symbol outside the requested nesting context" in {
    val config = JsonPathConfig(Seq("device", "location", "latitude"))
    test(new PushdownAutomaton(config)) { c =>
      val symbols = Seq(
        TestSymbol(SymbolKind.ObjOpen, 0),
        TestSymbol(SymbolKind.ObjOpen, 1),
        key(config, "latitude", 2),
        TestSymbol(SymbolKind.Colon, 10),
        TestSymbol(SymbolKind.ObjClose, 11),
        TestSymbol(SymbolKind.ObjClose, 12)
      )

      val (descriptors, endOfRecords) = send(c, symbols, config)
      c.io.found.expect(false.B)
      c.io.done.expect(true.B)
      assert(descriptors.isEmpty)
      assert(endOfRecords == 1)
    }
  }

  it should "hold the loader descriptor stable while the loader is stalled" in {
    val config = JsonPathConfig(Seq("temperature"), attributeId = 3)
    test(new PushdownAutomaton(config)) { c =>
      c.io.descriptor.ready.poke(false.B)
      val symbols = Seq(
        TestSymbol(SymbolKind.ObjOpen, 0),
        key(config, "temperature", 13),
        TestSymbol(SymbolKind.Colon, 14),
        TestSymbol(SymbolKind.ObjClose, 17)
      )

      c.io.in.valid.poke(true.B)
      c.io.in.bits.count.poke(2.U)
      for ((symbol, lane) <- symbols.take(2).zipWithIndex) {
        c.io.in.bits.entries(lane).kind.poke(symbol.kind)
        c.io.in.bits.entries(lane).keyId.poke(symbol.keyId.U)
        c.io.in.bits.entries(lane).addr.poke(symbol.address.U)
      }
      c.io.in.ready.expect(true.B)
      c.clock.step()

      for ((symbol, lane) <- symbols.drop(2).zipWithIndex) {
        c.io.in.bits.entries(lane).kind.poke(symbol.kind)
        c.io.in.bits.entries(lane).keyId.poke(symbol.keyId.U)
        c.io.in.bits.entries(lane).addr.poke(symbol.address.U)
      }
      c.io.descriptor.valid.expect(true.B)
      c.io.descriptor.bits.attributeId.expect(3.U)
      c.io.descriptor.bits.startAddress.expect(14.U)
      c.io.descriptor.bits.length.expect(3.U)
      c.io.in.ready.expect(false.B)
      for (_ <- 0 until 3) {
        c.io.descriptor.valid.expect(true.B)
        c.io.descriptor.bits.startAddress.expect(14.U)
        c.io.descriptor.bits.length.expect(3.U)
        c.clock.step()
      }
      c.io.descriptor.ready.poke(true.B)
      c.clock.step()
      c.io.found.expect(true.B)
    }
  }

  it should "connect directly to the two-symbol ChannelFifo output" in {
    val config = JsonPathConfig(Seq("device", "location", "latitude"), attributeId = 7)
    test(new PdaFifoHarness(config)) { c =>
      val symbols = Seq(
        TestSymbol(SymbolKind.ObjOpen, 0),
        key(config, "device", 8),
        TestSymbol(SymbolKind.Colon, 9),
        TestSymbol(SymbolKind.ObjOpen, 10),
        key(config, "location", 20),
        TestSymbol(SymbolKind.Colon, 21),
        TestSymbol(SymbolKind.ObjOpen, 22),
        key(config, "latitude", 32),
        TestSymbol(SymbolKind.Colon, 33),
        TestSymbol(SymbolKind.ObjClose, 38),
        TestSymbol(SymbolKind.ObjClose, 39),
        TestSymbol(SymbolKind.ObjClose, 40)
      )
      val batches = symbols.grouped(8).toSeq
      val descriptors = scala.collection.mutable.ArrayBuffer.empty[Result]
      var sent = 0
      var sawEndOfRecord = false
      var cycles = 0

      c.io.in.valid.poke(false.B)
      c.io.descriptor.ready.poke(true.B)
      while ((sent < batches.length || !sawEndOfRecord) && cycles < 100) {
        if (sent < batches.length) {
          val batch = batches(sent)
          c.io.in.valid.poke(true.B)
          c.io.in.bits.count.poke(batch.length.U)
          for (lane <- 0 until 8) {
            if (lane < batch.length) {
              val symbol = batch(lane)
              c.io.in.bits.entries(lane).kind.poke(symbol.kind)
              c.io.in.bits.entries(lane).keyId.poke(symbol.keyId.U)
              c.io.in.bits.entries(lane).addr.poke(symbol.address.U)
            } else {
              c.io.in.bits.entries(lane).kind.poke(SymbolKind.Comma)
              c.io.in.bits.entries(lane).keyId.poke(0.U)
              c.io.in.bits.entries(lane).addr.poke(0.U)
            }
          }
        } else {
          c.io.in.valid.poke(false.B)
        }

        if (sent < batches.length && c.io.in.ready.peek().litToBoolean) sent += 1
        if (c.io.descriptor.valid.peek().litToBoolean) {
          descriptors += Result(
            c.io.descriptor.bits.attributeId.peek().litValue,
            c.io.descriptor.bits.startAddress.peek().litValue,
            c.io.descriptor.bits.length.peek().litValue
          )
        }
        sawEndOfRecord = sawEndOfRecord || c.io.endOfRecord.peek().litToBoolean
        c.clock.step()
        cycles += 1
      }

      assert(sent == batches.length, "ChannelFifo did not accept all symbol batches")
      assert(sawEndOfRecord, "PDA did not signal end of record")
      assert(descriptors.toSeq == Seq(Result(7, 33, 5)))
    }
  }
}

/** Verifies that the PDA input is exactly the ChannelFifo's 2-wide output. */
class PdaFifoHarness(config: JsonPathConfig) extends Module {
  private val numKeys = config.keyIds.size
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new ChannelBatch(new JsonSymbol(numKeys, 12), width = 8)))
    val descriptor = Decoupled(new AttributeDescriptor)
    val endOfRecord = Output(Bool())
  })

  val fifo = Module(
    new ChannelFifo(
      new JsonSymbol(numKeys, 12),
      depth = 16,
      enqueueWidth = 8,
      dequeueWidth = 2
    )
  )
  val pda = Module(new PushdownAutomaton(config))

  fifo.io.in <> io.in
  pda.io.in <> fifo.io.out
  io.descriptor.valid := pda.io.descriptor.valid
  io.descriptor.bits := pda.io.descriptor.bits
  pda.io.descriptor.ready := io.descriptor.ready
  io.endOfRecord := pda.io.endOfRecord
}
