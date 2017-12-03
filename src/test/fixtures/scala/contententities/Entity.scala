package com.gu.contententity.thrift

case class Entity(
  id:           String,
  entityType:   EntityType,
  googleId:     Option[String]       = None,
  person:       Option[Person]       = None,
  film:         Option[Film]         = None,
  game:         Option[Game]         = None,
  restaurant:   Option[Restaurant]   = None,
  place:        Option[Place]        = None,
  organisation: Option[Organisation] = None) extends TStruct

object Entity extends TStructCodec[Entity] {
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
  implicit val r2 = new TFieldCodec[w2.T, EntityType]
  implicit val r3 = new TFieldCodec[w3.T, Option[String]]
  implicit val r4 = new TFieldCodec[w4.T, Option[Person]]
  implicit val r5 = new TFieldCodec[w5.T, Option[Film]]
  implicit val r6 = new TFieldCodec[w6.T, Option[Game]]
  implicit val r7 = new TFieldCodec[w7.T, Option[Restaurant]]
  implicit val r8 = new TFieldCodec[w8.T, Option[Place]]
  implicit val r9 = new TFieldCodec[w9.T, Option[Organisation]]
  override val defaults = HMap[TFieldCodec](
    w3.value -> None,
    w4.value -> None,
    w5.value -> None,
    w6.value -> None,
    w7.value -> None,
    w8.value -> None,
    w9.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.id,
    w2.value -> x.entityType,
    w3.value -> x.googleId,
    w4.value -> x.person,
    w5.value -> x.film,
    w6.value -> x.game,
    w7.value -> x.restaurant,
    w8.value -> x.place,
    w9.value -> x.organisation)
  override def decode = (m) ⇒ for {
    id ← m.get(w1.value).orElse(defaults.get(w1.value))
    entityType ← m.get(w2.value).orElse(defaults.get(w2.value))
    googleId ← m.get(w3.value).orElse(defaults.get(w3.value))
    person ← m.get(w4.value).orElse(defaults.get(w4.value))
    film ← m.get(w5.value).orElse(defaults.get(w5.value))
    game ← m.get(w6.value).orElse(defaults.get(w6.value))
    restaurant ← m.get(w7.value).orElse(defaults.get(w7.value))
    place ← m.get(w8.value).orElse(defaults.get(w8.value))
    organisation ← m.get(w9.value).orElse(defaults.get(w9.value))
  } yield Entity(
    id,
    entityType,
    googleId,
    person,
    film,
    game,
    restaurant,
    place,
    organisation)
}