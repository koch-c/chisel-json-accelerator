/** Configuration for the currently supported, object-key-only JSON path. */
final case class JsonPathConfig(
  path: Seq[String],
  maxDepth: Int = 8,
  attributeId: Int = 0
) {
  require(path.nonEmpty, "path must contain at least one object key")
  require(path.forall(_.nonEmpty), "path keys must not be empty")
  require(maxDepth >= 1, "maxDepth must be at least one")
  require(attributeId >= 0 && attributeId <= 255, "attributeId must fit in 8 bits")

  /** Stable key-symbol codes expected from the path encoder. */
  def keyIds: Map[String, Int] = path.distinct.zipWithIndex.toMap
}
