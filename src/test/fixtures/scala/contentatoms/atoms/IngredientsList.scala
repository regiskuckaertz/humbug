package com.gu.contentatom.thrift.atom.recipe

case class IngredientsList(
  title:       Option[String]   = None,
  ingredients: List[Ingredient]) extends TStruct

object IngredientsList extends TStructCodec[IngredientsList] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  implicit val r1 = new TFieldCodec[w1.T, Option[String]]
  implicit val r2 = new TFieldCodec[w2.T, List[Ingredient]]
  override val defaults = HMap[TFieldCodec](w1.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.title,
    w2.value -> x.ingredients)
  override def decode = (m) ⇒ for {
    title ← m.get(w1.value).orElse(defaults.get(w1.value))
    ingredients ← m.get(w2.value).orElse(defaults.get(w2.value))
  } yield IngredientsList(
    title,
    ingredients)
}