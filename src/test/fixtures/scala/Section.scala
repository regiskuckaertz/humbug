package com.gu.contentapi.client.model.v1

case class Section(
  id:                 String,
  webTitle:           String,
  webUrl:             String,
  apiUrl:             String,
  editions:           List[Edition],
  activeSponsorships: Option[List[Sponsorship]] = None) extends TStruct

object Section extends TStructCodec[Section] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  val w5 = Witness(5)
  val w6 = Witness(6)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, String]
  implicit val r3 = new TFieldCodec[w3.T, String]
  implicit val r4 = new TFieldCodec[w4.T, String]
  implicit val r5 = new TFieldCodec[w5.T, List[Edition]]
  implicit val r6 = new TFieldCodec[w6.T, Option[List[Sponsorship]]]
  override val defaults = HMap[TFieldCodec](w6.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.id,
    w2.value -> x.webTitle,
    w3.value -> x.webUrl,
    w4.value -> x.apiUrl,
    w5.value -> x.editions,
    w6.value -> x.activeSponsorships)
  override def decode = (m) ⇒ for {
    id ← m.get(w1.value).orElse(defaults.get(w1.value))
    webTitle ← m.get(w2.value).orElse(defaults.get(w2.value))
    webUrl ← m.get(w3.value).orElse(defaults.get(w3.value))
    apiUrl ← m.get(w4.value).orElse(defaults.get(w4.value))
    editions ← m.get(w5.value).orElse(defaults.get(w5.value))
    activeSponsorships ← m.get(w6.value).orElse(defaults.get(w6.value))
  } yield Section(
    id,
    webTitle,
    webUrl,
    apiUrl,
    editions,
    activeSponsorships)
}