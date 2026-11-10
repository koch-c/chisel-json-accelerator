package jsonaccel

import org.scalatest.flatspec.AnyFlatSpec

/** Drives AcceleratorClient through FakeTransport, so these tests check the
  * client's logic (writing, collecting, timing out) without a board. The reply
  * formats themselves are covered by ProtocolSpec.
  */
class AcceleratorClientSpec extends AnyFlatSpec {

  val json = """{ "temperature": 25, "humidity": 60 }"""

  def clientFor(transport: Transport, timeoutMillis: Int = 200): AcceleratorClient =
    new AcceleratorClient(transport, "temperature", timeoutMillis)

  /** Runs `body` and returns its result with the elapsed wall-clock time. */
  def timed[A](body: => A): (A, Long) = {
    val start = System.nanoTime()
    val result = body
    (result, (System.nanoTime() - start) / 1_000_000)
  }

  "AcceleratorClient" should "write the padded document exactly once" in {
    val transport = new FakeTransport(Seq("25\nFOUND\n"))
    clientFor(transport).query(json)
    assert(transport.written.toSeq == Protocol.encode(json).toSeq)
  }

  it should "return the value from a reply that arrives in one piece" in {
    val transport = new FakeTransport(Seq("25\nFOUND\n"))
    assert(clientFor(transport).query(json) == Found("25"))
  }

  it should "collect a reply that arrives in several chunks" in {
    val transport = new FakeTransport(Seq("2", "5\nFO", "UND\n"))
    assert(clientFor(transport).query(json) == Found("25"))
  }

  it should "report a missing key" in {
    val transport = new FakeTransport(Seq("NOT_", "FOUND\n"))
    assert(clientFor(transport).query(json) == NotFound)
  }

  it should "stop reading as soon as the reply is complete" in {
    val transport = new FakeTransport(Seq("25\nFOUND\n", "leftover"))
    assert(clientFor(transport).query(json) == Found("25"))
    assert(transport.unreadChunks == 1)
  }

  it should "pass a complete but unrecognised reply on as UnexpectedReply" in {
    val transport = new FakeTransport(Seq("xNOT_FOUND\n"))
    assert(clientFor(transport).query(json) == UnexpectedReply("xNOT_FOUND\n"))
  }

  it should "time out with nothing received when the board never answers" in {
    val transport = new FakeTransport(Seq.empty)
    val (result, elapsed) = timed(clientFor(transport, timeoutMillis = 100).query(json))
    assert(result == Timeout(""))
    assert(elapsed >= 100, s"gave up after only $elapsed ms")
    assert(elapsed < 1000, s"took $elapsed ms for a 100 ms timeout")
  }

  it should "keep what arrived before timing out" in {
    val transport = new FakeTransport(Seq("25\nFO"))
    assert(clientFor(transport, timeoutMillis = 100).query(json) == Timeout("25\nFO"))
  }

  it should "time out on a reply that never completes" in {
    val transport = new FakeTransport(Seq("hello"))
    assert(clientFor(transport, timeoutMillis = 100).query(json) == Timeout("hello"))
  }

  it should "apply the timeout to the whole reply, not to each read" in {
    // 200 chunks, 20 ms apart: each read finishes well within the timeout, but
    // the whole reply would take 4 seconds. The client must give up after
    // about 100 ms anyway.
    val transport = new FakeTransport(Seq.fill(200)("x"), delayMillis = 20)
    val (result, elapsed) = timed(clientFor(transport, timeoutMillis = 100).query(json))
    assert(result.isInstanceOf[Timeout], s"expected a Timeout, got $result")
    assert(elapsed < 1500, s"took $elapsed ms for a 100 ms timeout")
  }

  it should "time out on a second query, since the board needs a reset first" in {
    // The board stays silent after its first reply until it is reset. This
    // test expects the client to send the second document anyway and report
    // the silence as a timeout. If you decide the client should fail fast
    // instead, change this test to match.
    val transport = new FakeTransport(Seq("25\nFOUND\n"))
    val client = clientFor(transport, timeoutMillis = 100)
    assert(client.query(json) == Found("25"))
    assert(client.query(json) == Timeout(""))
  }
}
