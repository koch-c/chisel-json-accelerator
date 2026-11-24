import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.jdk.CollectionConverters._

class PushdownAutomatonThroughputSpec
    extends AnyFlatSpec
    with ChiselScalatestTester {
  private case class BenchSymbol(
      kind: SymbolKind.Type,
      address: Int,
      keyId: Int = 0
  )

  private case class Result(
      width: Int,
      matchEvery: Int,
      symbols: Int,
      batches: Int,
      cycles: Int,
      inputStallCycles: Int,
      descriptors: Int
  ) {
    private def symbolsPerCycle: Double = symbols.toDouble / cycles

    def report: String =
      f"PDA width $width (match every $matchEvery fields): " +
        f"symbols=$symbols, batches=$batches, cycles=$cycles, " +
        f"input stalls=$inputStallCycles, descriptors=$descriptors, " +
        f"throughput=$symbolsPerCycle%.3f symbols/cycle"

    def csvRow(runId: String): String =
      Seq(
        runId,
        width.toString,
        symbols.toString,
        batches.toString,
        cycles.toString,
        inputStallCycles.toString,
        descriptors.toString,
        symbolsPerCycle.toString
      ).mkString(",")
  }

  private val fieldCount = 64
  private val config = JsonPathConfig(Seq(Seq("v")))

  private val symbols = {
    val stream = scala.collection.mutable.ArrayBuffer(
      BenchSymbol(SymbolKind.ObjOpen, 0)
    )
    for (field <- 0 until fieldCount) {
      val keyAddress = 1 + field * 3
      stream += BenchSymbol(SymbolKind.Key, keyAddress, config.keyIds("v"))
      stream += BenchSymbol(SymbolKind.Colon, keyAddress + 1)
      val endKind =
        if (field == fieldCount - 1) SymbolKind.ObjClose else SymbolKind.Comma
      stream += BenchSymbol(endKind, keyAddress + 2)
    }
    stream.toSeq
  }

  private def benchmarkWidth(width: Int, matchEvery: Int = 1): Result = {
    val workloadSymbols = if (matchEvery == 1) {
      symbols
    } else {
      symbols.zipWithIndex.map {
        case (symbol, index)
            if symbol.kind == SymbolKind.Key && (index / 3) % matchEvery != 0 =>
          symbol.copy(keyId = 1)
        case (symbol, _) => symbol
      }
    }
    val batches = workloadSymbols.grouped(width).toSeq
    val keyIdWidth = math.max(1, chisel3.util.log2Ceil(config.keyIds.size))
    val expectedDescriptorCount =
      (0 until fieldCount).count(_ % matchEvery == 0)
    var result: Option[Result] = None

    test(new PushdownAutomaton(config, width)) { c =>
      c.io.in.valid.poke(false.B)
      c.io.in.bits.count.poke(0.U)
      c.io.descriptor.ready.poke(true.B)

      var sentBatches = 0
      var cycles = 0
      var inputStallCycles = 0
      var descriptorCount = 0
      var endOfRecords = 0
      val expectedDescriptors = (0 until fieldCount)
        .filter(_ % matchEvery == 0)
        .map(field => (2 + field * 3, 1))
      val observedDescriptors =
        scala.collection.mutable.ArrayBuffer.empty[(Int, Int)]
      val timeoutCycles = 10000

      while (
        (sentBatches < batches.length ||
          descriptorCount < expectedDescriptorCount ||
          endOfRecords == 0) &&
        cycles < timeoutCycles
      ) {
        val inputValid = sentBatches < batches.length
        c.io.in.valid.poke(inputValid.B)
        if (inputValid) {
          val batch = batches(sentBatches)
          c.io.in.bits.count.poke(batch.length.U)
          for (lane <- 0 until width) {
            if (lane < batch.length) {
              val symbol = batch(lane)
              c.io.in.bits.entries(lane).kind.poke(symbol.kind)
              c.io.in.bits
                .entries(lane)
                .keyId
                .poke(symbol.keyId.U(keyIdWidth.W))
              c.io.in.bits.entries(lane).addr.poke(symbol.address.U)
            } else {
              c.io.in.bits.entries(lane).kind.poke(SymbolKind.Comma)
              c.io.in.bits.entries(lane).keyId.poke(0.U)
              c.io.in.bits.entries(lane).addr.poke(0.U)
            }
          }
        }

        val inputFire = inputValid && c.io.in.ready.peek().litToBoolean
        if (inputValid && !c.io.in.ready.peek().litToBoolean)
          inputStallCycles += 1
        if (c.io.descriptor.valid.peek().litToBoolean) {
          observedDescriptors += ((
            c.io.descriptor.bits.startAddress.peek().litValue.toInt,
            c.io.descriptor.bits.length.peek().litValue.toInt
          ))
          descriptorCount += 1
        }
        if (c.io.endOfRecord.peek().litToBoolean) endOfRecords += 1

        c.clock.step()
        if (inputFire) sentBatches += 1
        cycles += 1
      }

      assert(cycles < timeoutCycles, "Timed out draining PDA batches")
      assert(
        sentBatches == batches.length,
        "Not all symbol batches were accepted"
      )
      assert(
        descriptorCount == expectedDescriptorCount,
        "PDA lost or duplicated a descriptor"
      )
      assert(observedDescriptors.toSeq == expectedDescriptors)
      assert(
        endOfRecords == 1,
        s"Expected one end-of-record pulse, got $endOfRecords"
      )
      c.io.done.expect(true.B)

      result = Some(
        Result(
          width = width,
          matchEvery = matchEvery,
          symbols = workloadSymbols.length,
          batches = batches.length,
          cycles = cycles,
          inputStallCycles = inputStallCycles,
          descriptors = descriptorCount
        )
      )
    }
    result.get
  }

  "PDA batch benchmark" should "compare sustained width-2, width-4, and width-8 input" in {
    val results = Seq(2, 4, 8).map(width => benchmarkWidth(width))
    results.foreach(result => info(result.report))

    val sparseResults = Seq(2, 4, 8).map(width => benchmarkWidth(width, 16))
    sparseResults.foreach(result => info(result.report))
    assert(
      sparseResults.last.cycles < sparseResults.head.cycles,
      "Expected width 8 to beat width 2 when descriptor production is sparse"
    )

    val outputPath = Paths.get(
      "target",
      "benchmarks",
      "pda-batch-throughput.csv"
    )
    Files.createDirectories(outputPath.getParent)
    val runId = java.time.Instant.now().toString
    val header =
      "run_id,pda_width,symbols,batches,cycles,input_stall_cycles,descriptors,symbols_per_cycle"
    val rows = results.map(_.csvRow(runId))

    if (Files.exists(outputPath)) {
      val existingLines =
        Files.readAllLines(outputPath, StandardCharsets.UTF_8).asScala.toSeq
      require(
        existingLines.headOption.contains(header),
        s"Unexpected PDA benchmark CSV header in $outputPath"
      )
      Files.write(
        outputPath,
        rows.asJava,
        StandardCharsets.UTF_8,
        StandardOpenOption.APPEND
      )
    } else {
      val csv = (Seq(header) ++ rows).mkString("\n") + "\n"
      Files.write(outputPath, csv.getBytes(StandardCharsets.UTF_8))
    }
    info(s"Appended PDA batch benchmark run $runId to $outputPath")
  }
}
