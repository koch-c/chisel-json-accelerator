package jsonaccel

import com.fazecast.jSerialComm.SerialPort
import java.io.IOException

/** A Transport backed by a serial port, using the jSerialComm library.
  *
  * @param portName "COM3" style on Windows, "/dev/ttyUSB1" style on Linux.
  *
*/
class SerialTransport(portName: String) extends Transport {

  private val port = SerialPort.getCommPort(portName)


  port.setComPortParameters(Protocol.BaudRate, 8, SerialPort.ONE_STOP_BIT, SerialPort.NO_PARITY)
  port.setComPortTimeouts(SerialPort.TIMEOUT_READ_SEMI_BLOCKING | SerialPort.TIMEOUT_WRITE_BLOCKING, 1000, 0)

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

  override def close(): Unit = {
    port.closePort()
  }
}
