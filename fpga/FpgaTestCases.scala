import java.nio.ByteBuffer
import java.nio.ByteOrder.LITTLE_ENDIAN
import java.nio.charset.StandardCharsets.UTF_8

/** The byte-level contract for the GenerateAcceleratorTop board profile. */
object FpgaTestCases {
  final case class Request(name: String, json: String, expected: Seq[Int])

  private def kept(id: Long, name: String, city: Option[String]): Seq[Int] = {
    val bytes = ByteBuffer.allocate(96).order(LITTLE_ENDIAN)
    def stringSlot(text: Option[String], capacity: Int): Unit = {
      val data = text.fold(Array.empty[Byte])(_.getBytes(UTF_8))
      bytes.putShort(data.length.toShort)
      bytes.put(data)
      bytes.put(new Array[Byte](capacity - data.length))
    }

    bytes.put(1.toByte)
    bytes.put((if (city.isDefined) 0x55 else 0x45).toByte)
    bytes.putLong(id)
    stringSlot(Some(name), 32)
    stringSlot(city, 32)
    stringSlot(Some("ready"), 16)
    require(!bytes.hasRemaining, "Incomplete FPGA response fixture")
    bytes.array().iterator.map(_ & 0xff).toVector
  }

  val requests: Seq[Request] = Seq(
    Request(
      "keep nested fields in configured order",
      """{"sensor":{"status":"ready"},"user":{"id":99,"profile":{"city":"Aarhus"},"name":"Ada"},"id":4294967303}""",
      kept(4294967303L, "Ada", Some("Aarhus"))
    ),
    Request(
      "drop one below the Int64 threshold",
      """{"id":4294967295,"user":{"name":"Ada"},"sensor":{"status":"ready"}}""",
      Seq(0)
    ),
    Request(
      "drop a status that only matches the prefix",
      """{"id":4294967303,"sensor":{"status":"ready!"}}""",
      Seq(0)
    ),
    Request(
      "keep at the threshold and clear the missing city slot",
      """{"user":{"name":"Bo"},"id":4294967296,"sensor":{"status":"ready"}}""",
      kept(4294967296L, "Bo", None)
    )
  )
}
