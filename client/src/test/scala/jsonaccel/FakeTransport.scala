package jsonaccel

import java.nio.charset.StandardCharsets
import scala.collection.mutable

/** A Transport for tests: it records what the client wrote and replays a
  * scripted reply.
  *
  * Each `read` hands out the next chunk, so a reply can arrive in pieces like
  * a real serial port delivers it. A chunk arrives `delayMillis` after the
  * read starts. If that is longer than the read's timeout, or no chunks are
  * left, the read waits out its timeout and returns nothing, like a silent
  * board.
  */
class FakeTransport(chunks: Seq[String], delayMillis: Int = 0) extends Transport {
  val written = mutable.ArrayBuffer[Byte]()
  var reads = 0
  var closed = false
  private val pending = mutable.Queue(chunks: _*)

  /** Chunks the client never read. */
  def unreadChunks: Int = pending.size

  override def write(bytes: Array[Byte]): Unit = written ++= bytes

  override def read(timeoutMillis: Int): Array[Byte] = {
    reads += 1
    if (pending.isEmpty || delayMillis > timeoutMillis) {
      Thread.sleep(math.max(timeoutMillis, 0).toLong)
      Array.emptyByteArray
    } else {
      Thread.sleep(delayMillis.toLong)
      pending.dequeue().getBytes(StandardCharsets.UTF_8)
    }
  }

  override def close(): Unit = closed = true
}
