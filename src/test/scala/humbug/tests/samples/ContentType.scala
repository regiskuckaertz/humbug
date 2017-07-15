package humbug
package tests
package samples

import humbug.codec._

sealed abstract class ContentType(override val value: Int) extends ThriftEnum(value)

object ContentType {
  case object ARTICLE     extends ContentType(0)
  case object LIVEBLOG    extends ContentType(1)
  case object GALLERY     extends ContentType(2)
  case object INTERACTIVE extends ContentType(3)
  case object PICTURE     extends ContentType(4)
  case object VIDEO       extends ContentType(5)
  case object CROSSWORD   extends ContentType(6)
  case object AUDIO       extends ContentType(7)

  implicit val reader = new ThriftEnumReader[ContentType] {
    def from(x: Int): Option[ContentType] = x match {
      case 0 => Some(ARTICLE)
      case 1 => Some(LIVEBLOG)
      case 2 => Some(GALLERY)
      case 3 => Some(INTERACTIVE)
      case 4 => Some(PICTURE)
      case 5 => Some(VIDEO)
      case 6 => Some(CROSSWORD)
      case 7 => Some(AUDIO)
      case _ => None
    }
  }
}
