package com.gu.contentapi.client.model.v1

case class ContentStats(videos: Int, images: Int, text: Int, tweets: Int, pullquotes: Int, audio: Int, interactives: Int, witness: Int, richlinks: Int, membership: Int, embeds: Int, comments: Int, instagram: Int, vines: Int) extends TStruct

object ContentStats extends TStructCodec[ContentStats] {
  val w1 = Witness(1)

  val w2 = Witness(2)

  val w3 = Witness(3)

  val w4 = Witness(4)

  val w5 = Witness(5)

  val w6 = Witness(6)

  val w7 = Witness(7)

  val w8 = Witness(8)

  val w9 = Witness(9)

  val w10 = Witness(1)

  val w11 = Witness(11)

  val w12 = Witness(21)

  val w13 = Witness(31)

  val w14 = Witness(41)

  implicit val r1 = new TFieldCodec[w1.T, Int]()

  implicit val r2 = new TFieldCodec[w2.T, Int]()

  implicit val r3 = new TFieldCodec[w3.T, Int]()

  implicit val r4 = new TFieldCodec[w4.T, Int]()

  implicit val r5 = new TFieldCodec[w5.T, Int]()

  implicit val r6 = new TFieldCodec[w6.T, Int]()

  implicit val r7 = new TFieldCodec[w7.T, Int]()

  implicit val r8 = new TFieldCodec[w8.T, Int]()

  implicit val r9 = new TFieldCodec[w9.T, Int]()

  implicit val r10 = new TFieldCodec[w10.T, Int]()

  implicit val r11 = new TFieldCodec[w11.T, Int]()

  implicit val r12 = new TFieldCodec[w12.T, Int]()

  implicit val r13 = new TFieldCodec[w13.T, Int]()

  implicit val r14 = new TFieldCodec[w14.T, Int]()

  override val defaults = HMap[TFieldCodec]()

  override def encode = (x) ⇒ HMap[TFieldCodec](w1.value -> x.videos, w2.value -> x.images, w3.value -> x.text, w4.value -> x.tweets, w5.value -> x.pullquotes, w6.value -> x.audio, w7.value -> x.interactives, w8.value -> x.witness, w9.value -> x.richlinks, w10.value -> x.membership, w11.value -> x.embeds, w12.value -> x.comments, w13.value -> x.instagram, w14.value -> x.vines)

  override def decode = (m) ⇒ for {
    videos ← m.get(w1.value).orElse(defaults.get(w1.value))

    images ← m.get(w2.value).orElse(defaults.get(w2.value))

    text ← m.get(w3.value).orElse(defaults.get(w3.value))

    tweets ← m.get(w4.value).orElse(defaults.get(w4.value))

    pullquotes ← m.get(w5.value).orElse(defaults.get(w5.value))

    audio ← m.get(w6.value).orElse(defaults.get(w6.value))

    interactives ← m.get(w7.value).orElse(defaults.get(w7.value))

    witness ← m.get(w8.value).orElse(defaults.get(w8.value))

    richlinks ← m.get(w9.value).orElse(defaults.get(w9.value))

    membership ← m.get(w10.value).orElse(defaults.get(w10.value))

    embeds ← m.get(w11.value).orElse(defaults.get(w11.value))

    comments ← m.get(w12.value).orElse(defaults.get(w12.value))

    instagram ← m.get(w13.value).orElse(defaults.get(w13.value))

    vines ← m.get(w14.value).orElse(defaults.get(w14.value))
  } yield ContentStats(videos, images, text, tweets, pullquotes, audio, interactives, witness, richlinks, membership, embeds, comments, instagram, vines)
}