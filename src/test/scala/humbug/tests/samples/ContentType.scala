package humbug
package tests
package samples

import humbug.codec._

sealed abstract class ContentType extends ThriftEnum
case object ARTICLE     extends ContentType
case object LIVEBLOG    extends ContentType
case object GALLERY     extends ContentType
case object INTERACTIVE extends ContentType
case object PICTURE     extends ContentType
case object VIDEO       extends ContentType
case object CROSSWORD   extends ContentType
case object AUDIO       extends ContentType

object ContentType {
  implicit codec = new ThriftEnumGeneric[ContentType] {
    def from: Int => Option[ContentType] = {
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

    def to: ContentType => Int = {
      case ARTICLE     => 0
      case LIVEBLOG    => 1
      case GALLERY     => 2
      case INTERACTIVE => 3
      case PICTURE     => 4
      case VIDEO       => 5
      case CROSSWORD   => 6
      case AUDIO       => 7
    }
  }
}
