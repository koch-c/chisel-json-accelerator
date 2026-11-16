package jsonaccel

import scala.collection.mutable.ArrayBuffer
import java.nio.charset.StandardCharsets

/** The public API: send a JSON document, get back a QueryResult.
  *
  * @param transport where the bytes go. A SerialTransport for the real board,
  *   or a fake one in tests.
  * @param key the key the bitstream was generated with. 
  *
  * @param timeoutMillis how long to wait for a full reply.
  *
  */
class AcceleratorClient(transport: Transport, val key: String, timeoutMillis: Int = 2000) {

  //Sends one document and waits for the board's reply. Encodes the document with `Protocol` and write it.
  
  def query(json: String): QueryResult = 
  {
    val encodedJson = Protocol.encode(json)

    transport.write(encodedJson)

    val deadline = System.nanoTime() + timeoutMillis * 1_000_000L

    val buf = ArrayBuffer[Byte]()

    while (!Protocol.isComplete(buf.toArray) && System.nanoTime() < deadline) {
      val remainingMillis = ((deadline - System.nanoTime()) / 1_000_000L).toInt
      buf ++= transport.read(math.max(1, remainingMillis))
    }

    if (Protocol.isComplete(buf.toArray)) Protocol.decode(buf.toArray)
    else Timeout(new String(buf.toArray, StandardCharsets.UTF_8))

  }


}
