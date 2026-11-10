package jsonaccel

/** Objects used for saving output from the board*/
sealed trait QueryResult

final case class Found(value: String) extends QueryResult

case object NotFound extends QueryResult


final case class Timeout(value: String) extends QueryResult


final case class UnexpectedReply(value: String) extends QueryResult



