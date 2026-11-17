package jsonaccel

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.util.{Failure, Success, Try, Using}

// Command-line entry point: sends one JSON file to the board and prints the reply.
  
//Run with `sbt "client/runMain jsonaccel.Main <port> <json-file>"`.

object Main {

  val ExitFound = 0
  val ExitNotFound = 1
  val ExitUsage = 2
  val ExitError = 3

  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      System.err.println("usage: Main <port> <json-file>")
      System.err.println("  port       COM4 style on Windows, /dev/ttyUSB1 style on Linux")
      System.err.println("  json-file  the document to send")
      sys.exit(ExitUsage)
    }

    val portName = args(0)
    val jsonPath = args(1)

    // Both steps can throw (missing file, port that won't open), so they run
    // inside a Try. Using closes the port whether the query succeeds or not.
    val result: Try[QueryResult] = for {
      json <- Try(Files.readString(Paths.get(jsonPath), StandardCharsets.UTF_8))
      reply <- Using(new SerialTransport(portName)) { transport =>
        new AcceleratorClient(transport).query(json)
      }
    } yield reply

    val exitCode = result match {
      case Success(Found(value)) =>
        println(value)
        ExitFound
      case Success(NotFound) =>
        System.err.println("""key not found""")
        ExitNotFound
      case Success(Timeout(received)) =>
        System.err.println(s"timed out waiting for the board (received so far: ${printable(received)})")
        System.err.println("the board only answers one document per reset; press the center button and try again")
        ExitError
      case Success(UnexpectedReply(received)) =>
        System.err.println(s"unexpected reply from the board: ${printable(received)}")
        ExitError
      case Failure(error) =>
        System.err.println(s"error: $error")
        ExitError
    }

    sys.exit(exitCode)
  }

  /** Makes a reply readable on one line by showing newlines as `\n`. */
  private def printable(text: String): String =
    "\"" + text.replace("\n", "\\n") + "\""
}
