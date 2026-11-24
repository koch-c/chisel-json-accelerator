//> using scala 2.13.16

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets.US_ASCII
import java.nio.file.Path
import java.nio.file.StandardOpenOption.{READ, WRITE}
import scala.sys.process._
import scala.util.Using
import scala.util.control.NonFatal

/** UART smoke test for the bitstream from GenerateAcceleratorTop (Linux). */
object TestFpga {
  private val record =
    """{"status":"ignore","sensor":{"status":null,"temperature":-12.5},"user":{"id":99,"profile":{"city":"Aarhus"},"active":true,"name":"Ada"},"id":7}""" + "\n"
  // Raw matches in input order, with no separators or trailing newline.
  private val expected =
    """:null:-12.5:"Aarhus":true:"Ada":7""".getBytes(US_ASCII)
  private val windowNanos = 1000000000L

  private def exchange(port: String): Array[Byte] =
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
        val request = ByteBuffer.wrap(record.getBytes(US_ASCII))
        while (request.hasRemaining) uart.write(request)
        val deadline = System.nanoTime() + windowNanos
        val received = new ByteArrayOutputStream()
        val buffer = ByteBuffer.allocate(expected.length + 1)
        // Keep reading after a full frame so a spurious second match fails.
        while (
          received.size() <= expected.length && System.nanoTime() - deadline < 0
        ) {
          buffer.clear()
          val count = uart.read(buffer)
          if (count > 0) received.write(buffer.array(), 0, count)
        }
        received.toByteArray
      } finally {
        // Some tty drivers report a partial restore even when the saved state matches.
        val status = Seq("stty", "-F", port, original).!(ProcessLogger(_ => ()))
        if (status != 0 && stty("-g") != original)
          throw new java.io.IOException(
            s"could not restore UART settings for $port"
          )
      }
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
    val received =
      try exchange(port)
      catch {

        case NonFatal(error) =>
          Console.err.println(s"FAIL: $port: ${error.getMessage}")
          sys.exit(1)
      }
    if (!received.sameElements(expected)) {
      Console.err.println(
        s"FAIL: $port: expected ${hex(expected)}, received ${hex(received)}"
      )
      sys.exit(1)
    }
    println(s"PASS: $port: six Raw matches: ${new String(received, US_ASCII)}")
  }
}
