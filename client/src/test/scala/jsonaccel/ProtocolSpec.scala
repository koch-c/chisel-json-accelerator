package jsonaccel

import java.nio.charset.StandardCharsets
import org.scalatest.flatspec.AnyFlatSpec

class ProtocolSpec extends AnyFlatSpec {

  def bytes(text: String): Array[Byte] = text.getBytes(StandardCharsets.UTF_8)

  "Protocol.encode" should "pad a document to a multiple of 8 bytes" in {
    val json = """{ "temperature": 25, "humidity": 60 }""" // 37 bytes
    val encoded = Protocol.encode(json)
    assert(encoded.length == 40)
    assert(encoded.toSeq == bytes(json + "   ").toSeq)
  }

  it should "not pad a document that is already a multiple of 8 bytes" in {
    val json = """{"temperature":25,"humidity":60,"ok":10}""" // 40 bytes
    assert(Protocol.encode(json).toSeq == bytes(json).toSeq)
    assert(Protocol.encode("").isEmpty)
  }

  it should "count bytes, not characters" in {
    val json = """{"city":"Århus"}""" // 16 characters, but Å is 2 bytes in UTF-8
    assert(json.length == 16)
    assert(bytes(json).length == 17)
    val encoded = Protocol.encode(json)
    assert(encoded.length == 24)
    assert(encoded.toSeq == bytes(json + " " * 7).toSeq)
  }

  "Protocol.isComplete" should "recognise both reply formats" in {
    assert(Protocol.isComplete(bytes("25\nFOUND\n")))
    assert(Protocol.isComplete(bytes("\"twenty five\"\nFOUND\n")))
    assert(Protocol.isComplete(bytes("NOT_FOUND\n")))
  }

  it should "not treat a partial reply as complete" in {
    for (partial <- Seq("", "2", "25", "25\n", "25\nFO", "25\nFOUND", "NOT_FOU", "NOT_FOUND")) {
      assert(!Protocol.isComplete(bytes(partial)), s"treated ${partial.replace("\n", "\\n")} as complete")
    }
  }

  "Protocol.decode" should "return the value of a found key" in {
    assert(Protocol.decode(bytes("25\nFOUND\n")) == Found("25"))
    assert(Protocol.decode(bytes("\"twenty five\"\nFOUND\n")) == Found("\"twenty five\""))
    assert(Protocol.decode(bytes("{\"a\":1}\nFOUND\n")) == Found("{\"a\":1}"))
  }

  it should "decode a value as UTF-8" in {
    assert(Protocol.decode(bytes("\"Århus\"\nFOUND\n")) == Found("\"Århus\""))
  }

  it should "report a missing key" in {
    assert(Protocol.decode(bytes("NOT_FOUND\n")) == NotFound)
  }

  it should "reject a reply that matches neither format" in {
    for (reply <- Seq("", "hello", "25\nFOUND", "FOUND\n", "NOT_FOUND", "xNOT_FOUND\n")) {
      assert(Protocol.decode(bytes(reply)) == UnexpectedReply(reply))
    }
  }
}
