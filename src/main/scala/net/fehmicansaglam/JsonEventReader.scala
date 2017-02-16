package net.fehmicansaglam

import net.fehmicansaglam.JsonEvent._

import scala.annotation.tailrec
import scala.io.Source

class JsonEventReader(source: Source) extends Iterator[JsonEvent] {

  private case object JsonKeyEnd extends JsonEvent

  private case object EOF extends JsonEvent

  // Initially set to the first event
  private[this] var lookAhead = if (source.hasNext) findNext(source.next()) else EOF

  @tailrec
  private[this] def findNext(ch: Char): JsonEvent = ch match {
    case '{' => JsonObjectStart
    case '}' => JsonObjectEnd
    case '[' => JsonArrayStart
    case ']' => JsonArrayEnd
    case ':' => JsonKeyEnd
    case '"' => JsonString(readString())
    case '-' => JsonNumber(BigDecimal(readNumber("-")))
    case _ if ch.isDigit => JsonNumber(BigDecimal(readNumber("" + ch)))
    case 't' =>
      // TODO check remaining characters
      source.drop(3)
      JsonBoolean(true)
    case 'f' =>
      // TODO check remaining characters
      source.drop(4)
      JsonBoolean(false)
    case 'n' =>
      // TODO check remaining characters
      source.drop(3)
      JsonNull
    case _ => if (source.hasNext) findNext(source.next()) else EOF
  }

  @tailrec
  private[this] def readString(str: String = ""): String = {
    if (source.isEmpty) str
    else {
      val c = source.next()
      if (c == '\\') readString(str + source.next())
      else if (c == '"') str
      else readString(str + c)
    }
  }

  @tailrec
  private[this] def readNumber(str: String): String = {
    if (source.isEmpty) str
    else {
      val c = source.next()
      if (c.isDigit || c == 'e' || c == 'E' || c == '+' || c == '-' || c == '.') readNumber(str + c)
      else str
    }
  }

  override def hasNext: Boolean = lookAhead != EOF

  override def next(): JsonEvent = {
    if (lookAhead == EOF) throw new NoSuchElementException
    val curr = lookAhead
    lookAhead = if (source.hasNext) findNext(source.next()) else EOF
    if (lookAhead == JsonKeyEnd) {
      lookAhead = findNext(source.next())
      curr match {
        case JsonString(value) => JsonKey(value)
        case _ => throw new RuntimeException("Expecting key")
      }
    } else {
      curr
    }
  }

}
