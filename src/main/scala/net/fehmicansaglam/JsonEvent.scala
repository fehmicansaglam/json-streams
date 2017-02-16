package net.fehmicansaglam

trait JsonEvent

object JsonEvent {

  case object JsonObjectStart extends JsonEvent

  case object JsonObjectEnd extends JsonEvent

  case object JsonArrayStart extends JsonEvent

  case object JsonArrayEnd extends JsonEvent

  case class JsonKey(value: String) extends JsonEvent

  case class JsonString(value: String) extends JsonEvent

  case class JsonNumber(value: BigDecimal) extends JsonEvent

  case class JsonBoolean(value: Boolean) extends JsonEvent

  case object JsonNull extends JsonEvent

}