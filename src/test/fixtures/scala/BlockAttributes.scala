package com.gu.contentapi.client.model.v1

case class BlockAttributes(
  keyEvent:              Option[Boolean]               = None,
  summary:               Option[Boolean]               = None,
  title:                 Option[String]                = None,
  pinned:                Option[Boolean]               = None,
  membershipPlaceholder: Option[MembershipPlaceholder] = None) extends TStruct

object BlockAttributes extends TStructCodec[BlockAttributes] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  val w5 = Witness(5)
  implicit val r1 = new TFieldCodec[w1.T, Option[Boolean]]
  implicit val r2 = new TFieldCodec[w2.T, Option[Boolean]]
  implicit val r3 = new TFieldCodec[w3.T, Option[String]]
  implicit val r4 = new TFieldCodec[w4.T, Option[Boolean]]
  implicit val r5 = new TFieldCodec[w5.T, Option[MembershipPlaceholder]]
  override val defaults = HMap[TFieldCodec](
    w1.value -> None,
    w2.value -> None,
    w3.value -> None,
    w4.value -> None,
    w5.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.keyEvent,
    w2.value -> x.summary,
    w3.value -> x.title,
    w4.value -> x.pinned,
    w5.value -> x.membershipPlaceholder)
  override def decode = (m) ⇒ for {
    keyEvent ← m.get(w1.value).orElse(defaults.get(w1.value))
    summary ← m.get(w2.value).orElse(defaults.get(w2.value))
    title ← m.get(w3.value).orElse(defaults.get(w3.value))
    pinned ← m.get(w4.value).orElse(defaults.get(w4.value))
    membershipPlaceholder ← m.get(w5.value).orElse(defaults.get(w5.value))
  } yield BlockAttributes(
    keyEvent,
    summary,
    title,
    pinned,
    membershipPlaceholder)
}