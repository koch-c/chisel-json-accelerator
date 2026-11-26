//> using scala 2.13.16
//> using file FpgaTestCases.scala

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path
import java.nio.file.StandardOpenOption.{READ, WRITE}
import scala.sys.process._
import scala.util.Using
import scala.util.control.NonFatal

/** UART regression test for the bitstream from GenerateAcceleratorTop (Linux).
  */
object TestFpga {
  private val windowNanos = 1000000000L

  private def exchange(port: String): Unit =
    Using.resource(FileChannel.open(Path.of(port), READ, WRITE)) { uart =>
      def stty(args: String*): String =
        (Seq("stty", "-F", port) ++ args).!!.trim

      val original = stty("-g")
      try {
        // Noncanonical reads return after 0.1 s of silence (VMIN=0, VTIME=1).
        stty(
          "raw",
          "-echo",
          "115200",
          "cs8",
          "-parenb",
          "-cstopb",
          "-crtscts",
          "-ixon",
          "-ixoff",
          "clocal",
          "cread",
          "min",
          "0",
          "time",
          "1"
        )
        for (request <- FpgaTestCases.requests) {
          val input = ByteBuffer.wrap((request.json + "\n").getBytes(UTF_8))
          while (input.hasRemaining) uart.write(input)
          val expected = request.expected.map(_.toByte).toArray
          val received = readResponse(uart, expected.length)
          if (!received.sameElements(expected)) {
            throw new java.io.IOException(
              s"${request.name}: expected ${hex(expected)}, received ${hex(received)}"
            )
          }
          println(s"PASS: ${request.name} (${received.length} bytes)")
        }
      } finally {
        // Some tty drivers report a partial restore even when the saved state matches.
        val status = Seq("stty", "-F", port, original).!(ProcessLogger(_ => ()))
        if (status != 0 && stty("-g") != original)
          throw new java.io.IOException(
            s"could not restore UART settings for $port"
          )
      }
    }

  private def readResponse(uart: FileChannel, size: Int): Array[Byte] = {
    val deadline = System.nanoTime() + windowNanos
    val received = new ByteArrayOutputStream()
    val buffer = ByteBuffer.allocate(size + 1)
    var quietAfterResponse = false
    // Wait for a quiet read after a full response to catch extra bytes before
    // sending the next line. Some TTYs surface a timed-out read as EOF.
    while (
      !quietAfterResponse && received.size() <= size && System
        .nanoTime() - deadline < 0
    ) {
      buffer.clear()
      val count = uart.read(buffer)
      if (count > 0) received.write(buffer.array(), 0, count)
      else if (received.size() == size) quietAfterResponse = true
    }
    received.toByteArray
  }

  private def hex(bytes: Array[Byte]): String =
    if (bytes.isEmpty) "(no bytes)"
    else bytes.map(b => f"${b & 0xff}%02x").mkString(" ")

  def main(args: Array[String]): Unit = {
    if (args.length > 1) {
      Console.err.println("Usage: TestFpga [/dev/ttyUSB0]")
      sys.exit(2)
    }
    val port = args.headOption.getOrElse("/dev/ttyUSB0")
    try exchange(port)
    catch {
      case NonFatal(error) =>
        Console.err.println(s"FAIL: $port: ${error.getMessage}")
        sys.exit(1)
    }
    println(s"PASS: $port: all ${FpgaTestCases.requests.size} requests")
  }
}
