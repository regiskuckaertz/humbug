package com.gu.contentapi.client.model.v1

case class PodcastCategory(main: String, sub: Option[String] = None) extends TStruct

object PodcastCategory extends TStructCodec[PodcastCategory] {
  val w1 = Witness(1)

  val w2 = Witness(2)

  implicit val r1 = new TFieldCodec[w1.T, String]()

  implicit val r2 = new TFieldCodec[w2.T, Option[String]]()

  override val defaults = HMap[TFieldCodec](w2.value -> None)

  override def encode = (x) ⇒ HMap[TFieldCodec](w1.value -> x.main, w2.value -> x.sub)

  override def decode = (m) ⇒ for {
    main ← m.get(w1.value).orElse(defaults.get(w1.value))

    sub ← m.get(w2.value).orElse(defaults.get(w2.value))
  } yield PodcastCategory(main, sub)
}