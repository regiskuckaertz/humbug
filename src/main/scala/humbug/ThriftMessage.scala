package humbug

abstract class ThriftMessage[A](
  val id: Int,
  val name: String,
  val value: A
)

object ThriftMessage {
  def apply[A](
    mtype: Int,
    mid: Int,
    mname: String,
    mvalue: A
  ): ThriftMessage[A] = mtype match {
    case 1 => ThriftCall(mid, mname, mvalue)
    case 2 => ThriftReply(mid, mname, mvalue)
    case 3 => ThriftException(mid, mname, mvalue)
    case 4 => ThriftOneWay(mid, mname, mvalue)
  }
}

case class ThriftCall[A](
  override val id: Int,
  override val name: String,
  override val value: A) extends ThriftMessage[A](id, name, value)

case class ThriftReply[A](
  override val id: Int,
  override val name: String,
  override val value: A) extends ThriftMessage[A](id, name, value)

case class ThriftException[A](
  override val id: Int,
  override val name: String,
  override val value: A) extends ThriftMessage[A](id, name, value)

case class ThriftOneWay[A](
  override val id: Int,
  override val name: String,
  override val value: A) extends ThriftMessage[A](id, name, value)
