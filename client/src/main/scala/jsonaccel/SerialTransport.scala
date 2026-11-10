package jsonaccel

// TODO: Uncomment once jSerialComm is added to build.sbt.
// import com.fazecast.jSerialComm.SerialPort

/** A Transport backed by a real serial port, using the jSerialComm library.
  *
  * @param portName "COM3" style on Windows, "/dev/ttyUSB1" style on Linux.
  *
  * HINT: Look up these parts of the jSerialComm API in its docs or javadoc:
  *   - `SerialPort.getCommPort(name)` and `SerialPort.getCommPorts()` (the
  *     second one is handy for listing ports when the name is wrong)
  *   - `setComPortParameters(baud, dataBits, stopBits, parity)`. This is
  *     where 8N1 from question 6 turns into code. Which constants mean
  *     "one stop bit" and "no parity"?
  *   - `setComPortTimeouts(mode, readTimeout, writeTimeout)`. Look up what
  *     `TIMEOUT_READ_SEMI_BLOCKING` means and why it fits `read` here.
  *   - `openPort()`, `writeBytes`, `readBytes`, `bytesAvailable` and
  *     `closePort()`. Several return a Boolean or -1 on failure; don't
  *     ignore those.
  */
class SerialTransport(portName: String) extends Transport {

  // TODO: Get the port, set 8N1 at Protocol.BaudRate, and open it.
  // HINT: If `openPort()` returns false, fail right here with a clear message
  // (for example by throwing an exception). Every later call would fail in a
  // more confusing way.

  override def write(bytes: Array[Byte]): Unit = ???

  override def read(timeoutMillis: Int): Array[Byte] = ???
  // HINT: Read into a buffer, then return only the bytes that actually
  // arrived. `readBytes` returns that count. `Array.take` or `slice` helps.

  override def close(): Unit = ???
}
