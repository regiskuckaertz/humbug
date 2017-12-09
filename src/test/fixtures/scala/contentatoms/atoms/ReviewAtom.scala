package com.gu.contentatom.thrift.atom.review

case class ReviewAtom(
  reviewType:      ReviewType,
  reviewer:        String,
  rating:          Rating,
  reviewSnippet:   String,
  entityId:        String,
  restaurant:      Option[Restaurant]  = None,
  game:            Option[Game]        = None,
  film:            Option[Film]        = None,
  sourceArticleId: Option[String]      = None,
  images:          Option[List[Image]] = None) extends TStruct

object ReviewAtom extends TStructCodec[ReviewAtom] {
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
  implicit val r1 = new TFieldCodec[w1.T, ReviewType]
  implicit val r2 = new TFieldCodec[w2.T, String]
  implicit val r3 = new TFieldCodec[w3.T, Rating]
  implicit val r4 = new TFieldCodec[w4.T, String]
  implicit val r5 = new TFieldCodec[w5.T, String]
  implicit val r6 = new TFieldCodec[w6.T, Option[Restaurant]]
  implicit val r7 = new TFieldCodec[w7.T, Option[Game]]
  implicit val r8 = new TFieldCodec[w8.T, Option[Film]]
  implicit val r9 = new TFieldCodec[w9.T, Option[String]]
  implicit val r10 = new TFieldCodec[w10.T, Option[List[Image]]]
  override val defaults = HMap[TFieldCodec](
    w6.value -> None,
    w7.value -> None,
    w8.value -> None,
    w9.value -> None,
    w10.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.reviewType,
    w2.value -> x.reviewer,
    w3.value -> x.rating,
    w4.value -> x.reviewSnippet,
    w5.value -> x.entityId,
    w6.value -> x.restaurant,
    w7.value -> x.game,
    w8.value -> x.film,
    w9.value -> x.sourceArticleId,
    w10.value -> x.images)
  override def decode = (m) ⇒ for {
    reviewType ← m.get(w1.value).orElse(defaults.get(w1.value))
    reviewer ← m.get(w2.value).orElse(defaults.get(w2.value))
    rating ← m.get(w3.value).orElse(defaults.get(w3.value))
    reviewSnippet ← m.get(w4.value).orElse(defaults.get(w4.value))
    entityId ← m.get(w5.value).orElse(defaults.get(w5.value))
    restaurant ← m.get(w6.value).orElse(defaults.get(w6.value))
    game ← m.get(w7.value).orElse(defaults.get(w7.value))
    film ← m.get(w8.value).orElse(defaults.get(w8.value))
    sourceArticleId ← m.get(w9.value).orElse(defaults.get(w9.value))
    images ← m.get(w10.value).orElse(defaults.get(w10.value))
  } yield ReviewAtom(
    reviewType,
    reviewer,
    rating,
    reviewSnippet,
    entityId,
    restaurant,
    game,
    film,
    sourceArticleId,
    images)
}