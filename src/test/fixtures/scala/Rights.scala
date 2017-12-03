package com.gu.contentapi.client.model.v1

case class Rights(
  syndicatable:          Option[Boolean] = Some(`false`),
  subscriptionDatabases: Option[Boolean] = Some(`false`),
  developerCommunity:    Option[Boolean] = Some(`false`)) extends TStruct

object Rights extends TStructCodec[Rights] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  implicit val r1 = new TFieldCodec[w1.T, Option[Boolean]]
  implicit val r2 = new TFieldCodec[w2.T, Option[Boolean]]
  implicit val r3 = new TFieldCodec[w3.T, Option[Boolean]]
  override val defaults = HMap[TFieldCodec](
    w1.value -> Some(`false`),
    w2.value -> Some(`false`),
    w3.value -> Some(`false`))
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.syndicatable,
    w2.value -> x.subscriptionDatabases,
    w3.value -> x.developerCommunity)
  override def decode = (m) ⇒ for {
    syndicatable ← m.get(w1.value).orElse(defaults.get(w1.value))
    subscriptionDatabases ← m.get(w2.value).orElse(defaults.get(w2.value))
    developerCommunity ← m.get(w3.value).orElse(defaults.get(w3.value))
  } yield Rights(
    syndicatable,
    subscriptionDatabases,
    developerCommunity)
}