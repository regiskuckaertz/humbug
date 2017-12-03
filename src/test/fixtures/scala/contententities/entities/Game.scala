package com.gu.contententity.thrift.entity.game

case class Game(
  title:      String,
  publisher:  Option[String] = None,
  platforms:  List[String],
  price:      Option[Price]  = None,
  pegiRating: Option[Int]    = None,
  genre:      List[String]) extends TStruct

object Game extends TStructCodec[Game] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  val w5 = Witness(5)
  val w6 = Witness(6)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, Option[String]]
  implicit val r3 = new TFieldCodec[w3.T, List[String]]
  implicit val r4 = new TFieldCodec[w4.T, Option[Price]]
  implicit val r5 = new TFieldCodec[w5.T, Option[Int]]
  implicit val r6 = new TFieldCodec[w6.T, List[String]]
  override val defaults = HMap[TFieldCodec](
    w2.value -> None,
    w4.value -> None,
    w5.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.title,
    w2.value -> x.publisher,
    w3.value -> x.platforms,
    w4.value -> x.price,
    w5.value -> x.pegiRating,
    w6.value -> x.genre)
  override def decode = (m) ⇒ for {
    title ← m.get(w1.value).orElse(defaults.get(w1.value))
    publisher ← m.get(w2.value).orElse(defaults.get(w2.value))
    platforms ← m.get(w3.value).orElse(defaults.get(w3.value))
    price ← m.get(w4.value).orElse(defaults.get(w4.value))
    pegiRating ← m.get(w5.value).orElse(defaults.get(w5.value))
    genre ← m.get(w6.value).orElse(defaults.get(w6.value))
  } yield Game(
    title,
    publisher,
    platforms,
    price,
    pegiRating,
    genre)
}