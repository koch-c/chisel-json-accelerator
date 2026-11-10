package jsonaccel

/** Moves raw bytes to and from the board. This is the only part of the client
  * that knows a serial port exists.
  *
  * Keeping it this small means a test can swap in a fake implementation (see
  * FakeTransport in the tests) and the client can't tell the difference.
  */
trait Transport extends AutoCloseable {

  /** Sends all of `bytes`. */
  def write(bytes: Array[Byte]): Unit

  /** Waits at most `timeoutMillis` for data and returns whatever arrived,
    * which may be nothing (an empty array) if the time ran out.
    *
    * HINT: Why return "whatever arrived" instead of "exactly n bytes"? Does
    * the client know in advance how long the reply will be?
    */
  def read(timeoutMillis: Int): Array[Byte]

  // HINT: `AutoCloseable` already declares `def close(): Unit`, so every
  // implementation must provide it. That lets callers write
  // `Using(transport) { t => ... }` with `scala.util.Using` and never leak an
  // open port.
}
