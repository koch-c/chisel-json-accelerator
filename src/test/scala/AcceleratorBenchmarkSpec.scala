import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import scala.jdk.CollectionConverters._

class AcceleratorBenchmarkSpec extends AnyFlatSpec with ChiselScalatestTester {
  private case class Result(
      pdaWidth: Int,
      name: String,
      inputBytes: Int,
      outputBytes: Int,
      cycles: Int,
      inputCycles: Int
  ) {
    private def fmt(value: Double): String =
      BigDecimal(value)
        .setScale(3, BigDecimal.RoundingMode.HALF_UP)
        .bigDecimal
        .toPlainString

    private val ingressRate = inputBytes.toDouble / inputCycles
    private val effectiveInputRate = inputBytes.toDouble / cycles
    private val outputRate = outputBytes.toDouble / cycles

    def report: String =
      s"$name (PDA width $pdaWidth): input=$inputBytes bytes, " +
        s"output=$outputBytes bytes, " +
        s"latency=$cycles cycles (${fmt(cycles / 100.0)} us @ 100 MHz), " +
        s"ingress=${fmt(ingressRate)} bytes/cycle, " +
        s"effective input=${fmt(effectiveInputRate)} bytes/cycle " +
        s"(${fmt(effectiveInputRate * 100.0)} MB/s), " +
        s"output=${fmt(outputRate)} bytes/cycle " +
        s"(${fmt(outputRate * 100.0)} MB/s)"

    def csvRow(runId: String): String =
      Seq(
        runId,
        pdaWidth.toString,
        name,
        inputBytes.toString,
        outputBytes.toString,
        cycles.toString,
        fmt(cycles / 100.0),
        fmt(ingressRate),
        fmt(effectiveInputRate),
        fmt(effectiveInputRate * 100.0),
        fmt(outputRate),
        fmt(outputRate * 100.0),
        "100"
      ).mkString(",")
  }

  private def benchmark(
      name: String,
      pdaWidth: Int,
      config: JsonPathConfig,
      json: String,
      expected: Seq[Int]
  ): Result = {
    val input = json.getBytes("UTF-8").map(_ & 0xff)
    val expectedOutput = expected

    var result: Option[Result] = None
    test(new Accelerator(config, pdaWidth = pdaWidth)) { c =>
      c.io.in.valid.poke(false.B)
      c.io.in.bits.count.poke(0.U)
      for (lane <- 0 until 8) c.io.in.bits.bytes(lane).poke(0.U)
      c.io.out.ready.poke(true.B)

      val output = scala.collection.mutable.ArrayBuffer.empty[Int]
      var inputIndex = 0
      var cycle = 0
      var firstInputCycle = -1
      var lastInputCycle = -1
      var doneCycle = -1
      val timeoutCycles = 20000

      while (doneCycle < 0 && cycle < timeoutCycles) {
        val inputValid = inputIndex < input.length
        val batch = input.slice(inputIndex, inputIndex + 8)
        c.io.in.valid.poke(inputValid.B)
        c.io.in.bits.count.poke((if (inputValid) batch.length else 0).U)
        for (lane <- 0 until 8) {
          c.io.in.bits
            .bytes(lane)
            .poke(
              (if (inputValid && lane < batch.length) batch(lane) else 0).U
            )
        }

        val inputFire =
          inputValid && c.io.in.ready.peek().litToBoolean
        if (inputFire) {
          if (firstInputCycle < 0) firstInputCycle = cycle
          lastInputCycle = cycle
          inputIndex += batch.length
        }

        if (c.io.out.valid.peek().litToBoolean) {
          output += c.io.out.bits.peek().litValue.toInt
        }
        if (c.io.done.peek().litToBoolean) doneCycle = cycle

        c.clock.step()
        cycle += 1
      }

      assert(doneCycle >= 0, s"Timed out after $timeoutCycles cycles")
      assert(
        inputIndex == input.length,
        "Record completed before all input bytes"
      )
      assert(output.toSeq == expectedOutput, s"Unexpected output: $output")

      result = Some(
        Result(
          pdaWidth = pdaWidth,
          name = name,
          inputBytes = inputIndex,
          outputBytes = output.length,
          cycles = doneCycle - firstInputCycle + 1,
          inputCycles = lastInputCycle - firstInputCycle + 1
        )
      )
    }
    result.get
  }

  "Accelerator benchmark" should "report repeatable core throughput and latency" in {
    val pdaWidths = Seq(2, 4, 8)
    val sparseJson =
      "{\"padding\":\"" + ("x" * 192) + "\",\"v\":7}\n"
    val results = pdaWidths.flatMap { pdaWidth =>
      Seq(
        benchmark(
          "sparse",
          pdaWidth,
          JsonPathConfig(Seq(FieldSpec(Seq("v")))),
          sparseJson,
          Seq(1, 1, 7, 0, 0, 0)
        ),
        benchmark(
          "dense_repeated_key",
          pdaWidth,
          JsonPathConfig(Seq(FieldSpec(Seq("v")))),
          "{" + Seq.fill(24)("\"v\":1").mkString(",") + "}\n",
          Seq(1, 1, 1, 0, 0, 0)
        ),
        benchmark(
          "multiple_paths",
          pdaWidth,
          JsonPathConfig(
            Seq(FieldSpec(Seq("id")), FieldSpec(Seq("user", "name")))
          ),
          "{\"id\":7,\"user\":{\"name\":\"bob\"}}\n",
          Seq(1, 9, 7, 0, 0, 0, 0, 0, 0, 0)
        )
      )
    }

    results.foreach(result => info(result.report))

    val outputPath = Paths.get(
      "target",
      "benchmarks",
      "accelerator-throughput.csv"
    )
    Files.createDirectories(outputPath.getParent)
    val runId = java.time.Instant.now().toString
    val header = Seq(
      "run_id",
      "pda_width",
      "workload",
      "input_bytes",
      "output_bytes",
      "latency_cycles",
      "latency_us_at_100MHz",
      "ingress_bytes_per_cycle",
      "effective_input_bytes_per_cycle",
      "effective_input_MB_per_s_at_100MHz",
      "output_bytes_per_cycle",
      "output_MB_per_s_at_100MHz",
      "assumed_clock_MHz"
    ).mkString(",")
    val rows = results.map(_.csvRow(runId))

    if (Files.exists(outputPath)) {
      val existingLines =
        Files.readAllLines(outputPath, StandardCharsets.UTF_8).asScala.toSeq
      if (existingLines.headOption.contains(header)) {
        Files.write(
          outputPath,
          rows.asJava,
          StandardCharsets.UTF_8,
          StandardOpenOption.APPEND
        )
      } else if (
        existingLines.headOption.exists(_.startsWith("run_id,workload,"))
      ) {
        // Preserve old runs, marking their unrecorded PDA width as unknown.
        val migratedRows = existingLines.tail.map { line =>
          val columns = line.split(",", 2)
          s"${columns(0)},unknown,${columns(1)}"
        }
        val migrated =
          (Seq(header) ++ migratedRows ++ rows).mkString("\n") + "\n"
        Files.write(outputPath, migrated.getBytes(StandardCharsets.UTF_8))
      } else if (existingLines.headOption.exists(_.startsWith("workload,"))) {
        // Retain the initial file written before run IDs and widths were added.
        val previousRunId =
          Files.getLastModifiedTime(outputPath).toInstant.toString
        val migratedRows =
          existingLines.tail.map(line => s"$previousRunId,unknown,$line")
        val migrated =
          (Seq(header) ++ migratedRows ++ rows).mkString("\n") + "\n"
        Files.write(outputPath, migrated.getBytes(StandardCharsets.UTF_8))
      } else {
        throw new IllegalStateException(
          s"Unexpected benchmark CSV header in $outputPath"
        )
      }
    } else {
      val csv = (Seq(header) ++ rows).mkString("\n") + "\n"
      Files.write(outputPath, csv.getBytes(StandardCharsets.UTF_8))
    }
    info(s"Appended benchmark run $runId to ${outputPath.toString}")
  }
}
