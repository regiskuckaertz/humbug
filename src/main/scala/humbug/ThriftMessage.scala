package humbug

abstract class ThriftMessageType
case object ThriftCall extends ThriftMessageType
case object ThriftReply extends ThriftMessageType
case object ThriftException extends ThriftMessageType
case object ThriftOneWay extends ThriftMessageType

class ThriftMessage[A](
  val id: Int,
  val mtype: ThriftMessageType,
  val name: String,
  val value: A
)

object ThriftMessage {
  def apply[A](
    mtype: ThriftMessageType,
    mid: Int,
    mname: String,
    mvalue: A
  ): ThriftMessage[A] = new ThriftMessage(
    mid, mtype, mname, mvalue
  )
}

