package humbug

abstract class ThriftMessageType(val value: Byte)
final case object ThriftCall extends ThriftMessageType(1)
final case object ThriftReply extends ThriftMessageType(2)
final case object ThriftException extends ThriftMessageType(3)
final case object ThriftOneWay extends ThriftMessageType(4)

object ThriftMessageType {
  def apply(x: Int): Option[ThriftMessageType] = x match {
    case 1 ⇒ Some(ThriftCall)
    case 2 ⇒ Some(ThriftReply)
    case 3 ⇒ Some(ThriftException)
    case 4 ⇒ Some(ThriftOneWay)
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
  def apply[A](
    id:     Int,
    `type`: ThriftMessageType,
    name:   String,
    value:  A
  ): ThriftMessage[A] = new ThriftMessage(
    id, `type`, name, value
  )
}

