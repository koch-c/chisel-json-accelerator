package jsonaccel

import java.nio.charset.StandardCharsets

// encoding and decoding for the accelerator's UART protocol.

object Protocol {

  val BaudRate: Int = 115_200
  val WordSize: Int = 8
  val FoundSuffix: String = "\nFOUND\n"
  val MissingReply: String = "NOT_FOUND\n"

  /** Turns a JSON document into the exact bytes to write to the UART:
    * UTF-8 encoded and space-padded to a multiple of `WordSize` bytes.
    */
  def encode(json: String): Array[Byte] = {
    // Pad the bytes
    val bytes = json.getBytes(StandardCharsets.UTF_8)

    val padding = (WordSize - bytes.length % WordSize) % WordSize
    bytes ++ Array.fill(padding)(' '.toByte)
  }

  /** True once `received` holds a complete reply and the client can stop
    * reading.
    */
  def isComplete(received: Array[Byte]): Boolean = {
    val text = new String(received, StandardCharsets.UTF_8)
    text.endsWith(FoundSuffix) || text.endsWith(MissingReply)
  }

  //Turns a complete reply into a result.
  def decode(reply: Array[Byte]): QueryResult = {
    val text = new String(reply, StandardCharsets.UTF_8)
    // Check for an exact match, not endsWith: noise before "NOT_FOUND\n" means
    // the reply can't be trusted.
    if (text == MissingReply) NotFound
    else if (text.endsWith(FoundSuffix)) Found(text.stripSuffix(FoundSuffix))
    else UnexpectedReply(text)
  }
}
