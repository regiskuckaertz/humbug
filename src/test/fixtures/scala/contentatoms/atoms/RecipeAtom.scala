package com.gu.contentatom.thrift.atom.recipe

case class RecipeAtom(
  title:            String,
  tags:             Tags,
  time:             Time,
  serves:           Option[Serves]        = None,
  ingredientsLists: List[IngredientsList],
  steps:            List[String],
  credits:          List[String],
  images:           List[Image],
  sourceArticleId:  Option[String]        = None) extends TStruct

object RecipeAtom extends TStructCodec[RecipeAtom] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  val w5 = Witness(5)
  val w6 = Witness(6)
  val w7 = Witness(7)
  val w8 = Witness(8)
  val w9 = Witness(9)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, Tags]
  implicit val r3 = new TFieldCodec[w3.T, Time]
  implicit val r4 = new TFieldCodec[w4.T, Option[Serves]]
  implicit val r5 = new TFieldCodec[w5.T, List[IngredientsList]]
  implicit val r6 = new TFieldCodec[w6.T, List[String]]
  implicit val r7 = new TFieldCodec[w7.T, List[String]]
  implicit val r8 = new TFieldCodec[w8.T, List[Image]]
  implicit val r9 = new TFieldCodec[w9.T, Option[String]]
  override val defaults = HMap[TFieldCodec](
    w4.value -> None,
    w9.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.title,
    w2.value -> x.tags,
    w3.value -> x.time,
    w4.value -> x.serves,
    w5.value -> x.ingredientsLists,
    w6.value -> x.steps,
    w7.value -> x.credits,
    w8.value -> x.images,
    w9.value -> x.sourceArticleId)
  override def decode = (m) ⇒ for {
    title ← m.get(w1.value).orElse(defaults.get(w1.value))
    tags ← m.get(w2.value).orElse(defaults.get(w2.value))
    time ← m.get(w3.value).orElse(defaults.get(w3.value))
    serves ← m.get(w4.value).orElse(defaults.get(w4.value))
    ingredientsLists ← m.get(w5.value).orElse(defaults.get(w5.value))
    steps ← m.get(w6.value).orElse(defaults.get(w6.value))
    credits ← m.get(w7.value).orElse(defaults.get(w7.value))
    images ← m.get(w8.value).orElse(defaults.get(w8.value))
    sourceArticleId ← m.get(w9.value).orElse(defaults.get(w9.value))
  } yield RecipeAtom(
    title,
    tags,
    time,
    serves,
    ingredientsLists,
    steps,
    credits,
    images,
    sourceArticleId)
}