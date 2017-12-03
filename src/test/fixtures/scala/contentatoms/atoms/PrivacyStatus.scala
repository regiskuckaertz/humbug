package contentatom.media

sealed trait PrivacyStatus extends TEnum
case object PRIVATE extends PrivacyStatus
case object UNLISTED extends PrivacyStatus
case object PUBLIC extends PrivacyStatus

object PrivacyStatus extends TEnumCodec[PrivacyStatus] {
  override def encode = {
    case PRIVATE  ⇒ 0
    case UNLISTED ⇒ 1
    case PUBLIC   ⇒ 2
  }
  override def decode = {
    case 0 ⇒ PRIVATE
    case 1 ⇒ UNLISTED
    case 2 ⇒ PUBLIC
  }
}