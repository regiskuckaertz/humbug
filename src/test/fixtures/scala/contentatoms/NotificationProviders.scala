package com.gu.contentatom.thrift

case class NotificationProviders(email: Option[EmailProvider] = None) extends TStruct

object NotificationProviders extends TStructCodec[NotificationProviders] {
  val w1 = Witness(1)
  implicit val r1 = new TFieldCodec[w1.T, Option[EmailProvider]]
  override val defaults = HMap[TFieldCodec](w1.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](w1.value -> x.email)
  override def decode = (m) ⇒ for {
    email ← m.get(w1.value).orElse(defaults.get(w1.value))
  } yield NotificationProviders(email)
}