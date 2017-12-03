package com.gu.contentapi.client.model.v1

case class Asset(
  `type`:   AssetType,
  mimeType: Option[String]      = None,
  file:     Option[String]      = None,
  typeData: Option[AssetFields] = None) extends TStruct

object Asset extends TStructCodec[Asset] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  implicit val r1 = new TFieldCodec[w1.T, AssetType]
  implicit val r2 = new TFieldCodec[w2.T, Option[String]]
  implicit val r3 = new TFieldCodec[w3.T, Option[String]]
  implicit val r4 = new TFieldCodec[w4.T, Option[AssetFields]]
  override val defaults = HMap[TFieldCodec](
    w2.value -> None,
    w3.value -> None,
    w4.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.`type`,
    w2.value -> x.mimeType,
    w3.value -> x.file,
    w4.value -> x.typeData)
  override def decode = (m) ⇒ for {
    `type` ← m.get(w1.value).orElse(defaults.get(w1.value))
    mimeType ← m.get(w2.value).orElse(defaults.get(w2.value))
    file ← m.get(w3.value).orElse(defaults.get(w3.value))
    typeData ← m.get(w4.value).orElse(defaults.get(w4.value))
  } yield Asset(
    `type`,
    mimeType,
    file,
    typeData)
}