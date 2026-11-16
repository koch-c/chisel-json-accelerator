package jsonaccel

import com.fazecast.jSerialComm.SerialPort
import java.io.IOException

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

  private val port = SerialPort.getCommPort(portName)


  port.setComPortParameters(Protocol.BaudRate, 8, SerialPort.ONE_STOP_BIT, SerialPort.NO_PARITY)
  port.setComPortTimeouts(SerialPort.TIMEOUT_READ_SEMI_BLOCKING | SerialPort.TIMEOUT_WRITE_BLOCKING, 1000, 0)

  // TODO: Get the port, set 8N1 at Protocol.BaudRate, and open it.
  // HINT: If `openPort()` returns false, fail right here with a clear message
  // (for example by throwing an exception). Every later call would fail in a
  // more confusing way.

  if(!port.openPort()){
    throw new IOException(s"Something went wrong: ${SerialPort.getCommPorts().map(_.getSystemPortName).mkString(", ")}")
  }

  override def write(bytes: Array[Byte]): Unit = 
  {
    val byteLength = port.writeBytes(bytes, bytes.length)

    if(byteLength != bytes.length){
      throw new IOException(s"Wrote only $byteLength of ${bytes.length} bytes to $portName")
    }
  }

  override def read(timeoutMillis: Int): Array[Byte] = 
  {
    port.setComPortTimeouts(
      SerialPort.TIMEOUT_READ_SEMI_BLOCKING | SerialPort.TIMEOUT_WRITE_BLOCKING,
      timeoutMillis, 0)
    
    val buf = new Array[Byte](256)

    val n = port.readBytes(buf, buf.length)

    if(n > 0){
      buf.take(n)
    }else if(n == 0){
      Array.emptyByteArray
    }else if(n == -1) {
      throw new IOException(s"$portName is either wrong or unplugged")
    } else {
      throw new IOException("Else statement reached")
    }

    
  }
  // HINT: Read into a buffer, then return only the bytes that actually
  // arrived. `readBytes` returns that count. `Array.take` or `slice` helps.

  override def close(): Unit = {
    port.closePort()
  }
}
