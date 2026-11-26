sealed trait CompareOp
object CompareOp {
  case object Eq extends CompareOp
  case object Lt extends CompareOp
  case object Le extends CompareOp
  case object Gt extends CompareOp
  case object Ge extends CompareOp
}

sealed trait FilterValue
final case class IntLit(value: BigInt) extends FilterValue
final case class StrLit(value: String) extends FilterValue
