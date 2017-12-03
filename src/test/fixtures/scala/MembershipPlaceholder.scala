package com.gu.contentapi.client.model.v1

case class MembershipPlaceholder(campaignCode: Option[String] = None) extends TStruct

object MembershipPlaceholder extends TStructCodec[MembershipPlaceholder] {
  val w1 = Witness(1)
  implicit val r1 = new TFieldCodec[w1.T, Option[String]]
  override val defaults = HMap[TFieldCodec](w1.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](w1.value -> x.campaignCode)
  override def decode = (m) ⇒ for {
    campaignCode ← m.get(w1.value).orElse(defaults.get(w1.value))
  } yield MembershipPlaceholder(campaignCode)
}