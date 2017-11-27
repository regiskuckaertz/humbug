package com.gu.contentapi.client.model.v1

case class NetworkFront(id: String, path: String, edition: String, webTitle: String, webUrl: String, apiUrl: String) extends TStruct

object NetworkFront extends TStructCodec[NetworkFront] {
  val w1 = Witness(1)

  val w2 = Witness(2)

  val w3 = Witness(3)

  val w4 = Witness(4)

  val w5 = Witness(5)

  val w6 = Witness(6)

  implicit val r1 = new TFieldCodec[w1.T, String]()

  implicit val r2 = new TFieldCodec[w2.T, String]()

  implicit val r3 = new TFieldCodec[w3.T, String]()

  implicit val r4 = new TFieldCodec[w4.T, String]()

  implicit val r5 = new TFieldCodec[w5.T, String]()

  implicit val r6 = new TFieldCodec[w6.T, String]()

  override val defaults = HMap[TFieldCodec]()

  override def encode = (x) ⇒ HMap[TFieldCodec](w1.value -> x.id, w2.value -> x.path, w3.value -> x.edition, w4.value -> x.webTitle, w5.value -> x.webUrl, w6.value -> x.apiUrl)

  override def decode = (m) ⇒ for {
    id ← m.get(w1.value).orElse(defaults.get(w1.value))

    path ← m.get(w2.value).orElse(defaults.get(w2.value))

    edition ← m.get(w3.value).orElse(defaults.get(w3.value))

    webTitle ← m.get(w4.value).orElse(defaults.get(w4.value))

    webUrl ← m.get(w5.value).orElse(defaults.get(w5.value))

    apiUrl ← m.get(w6.value).orElse(defaults.get(w6.value))
  } yield NetworkFront(id, path, edition, webTitle, webUrl, apiUrl)
}