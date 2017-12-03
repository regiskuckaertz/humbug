package contentatom.media

sealed trait Platform extends TEnum
case object YOUTUBE extends Platform
case object FACEBOOK extends Platform
case object DAILYMOTION extends Platform
case object MAINSTREAM extends Platform
case object URL extends Platform

object Platform extends TEnumCodec[Platform] {
  override def encode = {
    case YOUTUBE     ⇒ 0
    case FACEBOOK    ⇒ 1
    case DAILYMOTION ⇒ 2
    case MAINSTREAM  ⇒ 3
    case URL         ⇒ 4
  }
  override def decode = {
    case 0 ⇒ YOUTUBE
    case 1 ⇒ FACEBOOK
    case 2 ⇒ DAILYMOTION
    case 3 ⇒ MAINSTREAM
    case 4 ⇒ URL
  }
}