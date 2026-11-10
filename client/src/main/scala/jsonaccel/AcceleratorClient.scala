package jsonaccel

/** The public API: send a JSON document, get back a QueryResult.
  *
  * @param transport where the bytes go. A SerialTransport for the real board,
  *   or a fake one in tests.
  * @param key the key the bitstream was generated with. Why is it here and not
  *   a parameter of `query`? (Question 4.) Does the client need it at all, or
  *   is it only useful as documentation and in error messages?
  * @param timeoutMillis how long to wait for a full reply.
  *
  * TODO: Document the "one document per reset" rule (question 5) in this
  * comment. A caller who misses it will only ever see timeouts.
  */
class AcceleratorClient(transport: Transport, val key: String, timeoutMillis: Int = 2000) {

  /** Sends one document and waits for the board's reply.
    *
    * TODO: Implement this. The steps are:
    *   1. Encode the document with `Protocol` and write it.
    *   2. Keep reading and collecting bytes until `Protocol.isComplete` says
    *      the reply is complete, or until the time is up.
    *   3. Decode the complete reply with `Protocol`, or report a timeout.
    *
    * HINT: For step 2, compute a deadline once (`System.nanoTime` or
    * `System.currentTimeMillis`) and give each `transport.read` call only the
    * remaining time. Why is "timeoutMillis per read call" the wrong timeout?
    * Think about a board that sends one byte every 1.9 seconds.
    *
    * HINT: An `ArrayBuffer[Byte]` (from `scala.collection.mutable`) or `++` on
    * an Array works for collecting the bytes. This is one of the few places
    * where a `var` or a mutable buffer with a `while` loop is perfectly fine
    * Scala.
    *
    * HINT: What should a second `query` on the same client do without a
    * reset in between? You could track that in a field and fail fast with a
    * helpful message, or leave it to the timeout. Either is a valid choice;
    * just make it deliberate.
    */
  def query(json: String): QueryResult = ???
}
