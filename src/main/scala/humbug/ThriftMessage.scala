package humbug

abstract class ThriftMessageType(val value: Byte)
final case object ThriftMessageCall extends ThriftMessageType(1)
final case object ThriftMessageReply extends ThriftMessageType(2)
final case object ThriftMessageException extends ThriftMessageType(3)
final case object ThriftMessageOneWay extends ThriftMessageType(4)

object ThriftMessageType {
  def apply(x: Int): Option[ThriftMessageType] = x match {
    case 1 ⇒ Some(ThriftMessageCall)
    case 2 ⇒ Some(ThriftMessageReply)
    case 3 ⇒ Some(ThriftMessageException)
    case 4 ⇒ Some(ThriftMessageOneWay)
    case _ ⇒ None
  }
}

class ThriftMessage[A](
    val id:     Int,
    val `type`: ThriftMessageType,
    val name:   String,
    val value:  A
)

object ThriftMessage {
  def apply[A](id: Int, `type`: ThriftMessageType, name: String, value: A): ThriftMessage[A] =
    new ThriftMessage(id, `type`, name, value)
}

