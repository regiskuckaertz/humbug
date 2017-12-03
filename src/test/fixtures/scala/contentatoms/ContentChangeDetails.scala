package com.gu.contentatom.thrift

case class ContentChangeDetails(
  lastModified:    Option[ChangeRecord] = None,
  created:         Option[ChangeRecord] = None,
  published:       Option[ChangeRecord] = None,
  revision:        Long,
  takenDown:       Option[ChangeRecord] = None,
  scheduledLaunch: Option[ChangeRecord] = None,
  embargo:         Option[ChangeRecord] = None,
  expiry:          Option[ChangeRecord] = None) extends TStruct

object ContentChangeDetails extends TStructCodec[ContentChangeDetails] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  val w5 = Witness(5)
  val w6 = Witness(6)
  val w7 = Witness(7)
  val w8 = Witness(9)
  implicit val r1 = new TFieldCodec[w1.T, Option[ChangeRecord]]
  implicit val r2 = new TFieldCodec[w2.T, Option[ChangeRecord]]
  implicit val r3 = new TFieldCodec[w3.T, Option[ChangeRecord]]
  implicit val r4 = new TFieldCodec[w4.T, Long]
  implicit val r5 = new TFieldCodec[w5.T, Option[ChangeRecord]]
  implicit val r6 = new TFieldCodec[w6.T, Option[ChangeRecord]]
  implicit val r7 = new TFieldCodec[w7.T, Option[ChangeRecord]]
  implicit val r8 = new TFieldCodec[w8.T, Option[ChangeRecord]]
  override val defaults = HMap[TFieldCodec](
    w1.value -> None,
    w2.value -> None,
    w3.value -> None,
    w5.value -> None,
    w6.value -> None,
    w7.value -> None,
    w8.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.lastModified,
    w2.value -> x.created,
    w3.value -> x.published,
    w4.value -> x.revision,
    w5.value -> x.takenDown,
    w6.value -> x.scheduledLaunch,
    w7.value -> x.embargo,
    w8.value -> x.expiry)
  override def decode = (m) ⇒ for {
    lastModified ← m.get(w1.value).orElse(defaults.get(w1.value))
    created ← m.get(w2.value).orElse(defaults.get(w2.value))
    published ← m.get(w3.value).orElse(defaults.get(w3.value))
    revision ← m.get(w4.value).orElse(defaults.get(w4.value))
    takenDown ← m.get(w5.value).orElse(defaults.get(w5.value))
    scheduledLaunch ← m.get(w6.value).orElse(defaults.get(w6.value))
    embargo ← m.get(w7.value).orElse(defaults.get(w7.value))
    expiry ← m.get(w8.value).orElse(defaults.get(w8.value))
  } yield ContentChangeDetails(
    lastModified,
    created,
    published,
    revision,
    takenDown,
    scheduledLaunch,
    embargo,
    expiry)
}