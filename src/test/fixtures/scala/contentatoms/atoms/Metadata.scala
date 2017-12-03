package contentatom.media

case class Metadata(
  tags:            Option[List[String]]  = None,
  categoryId:      Option[String]        = None,
  license:         Option[String]        = None,
  commentsEnabled: Option[Boolean]       = None,
  channelId:       Option[String]        = None,
  privacyStatus:   Option[PrivacyStatus] = None,
  expiryDate:      Option[DateTime]      = None,
  pluto:           Option[PlutoData]     = None) extends TStruct

object Metadata extends TStructCodec[Metadata] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  val w5 = Witness(5)
  val w6 = Witness(6)
  val w7 = Witness(7)
  val w8 = Witness(8)
  implicit val r1 = new TFieldCodec[w1.T, Option[List[String]]]
  implicit val r2 = new TFieldCodec[w2.T, Option[String]]
  implicit val r3 = new TFieldCodec[w3.T, Option[String]]
  implicit val r4 = new TFieldCodec[w4.T, Option[Boolean]]
  implicit val r5 = new TFieldCodec[w5.T, Option[String]]
  implicit val r6 = new TFieldCodec[w6.T, Option[PrivacyStatus]]
  implicit val r7 = new TFieldCodec[w7.T, Option[DateTime]]
  implicit val r8 = new TFieldCodec[w8.T, Option[PlutoData]]
  override val defaults = HMap[TFieldCodec](
    w1.value -> None,
    w2.value -> None,
    w3.value -> None,
    w4.value -> None,
    w5.value -> None,
    w6.value -> None,
    w7.value -> None,
    w8.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.tags,
    w2.value -> x.categoryId,
    w3.value -> x.license,
    w4.value -> x.commentsEnabled,
    w5.value -> x.channelId,
    w6.value -> x.privacyStatus,
    w7.value -> x.expiryDate,
    w8.value -> x.pluto)
  override def decode = (m) ⇒ for {
    tags ← m.get(w1.value).orElse(defaults.get(w1.value))
    categoryId ← m.get(w2.value).orElse(defaults.get(w2.value))
    license ← m.get(w3.value).orElse(defaults.get(w3.value))
    commentsEnabled ← m.get(w4.value).orElse(defaults.get(w4.value))
    channelId ← m.get(w5.value).orElse(defaults.get(w5.value))
    privacyStatus ← m.get(w6.value).orElse(defaults.get(w6.value))
    expiryDate ← m.get(w7.value).orElse(defaults.get(w7.value))
    pluto ← m.get(w8.value).orElse(defaults.get(w8.value))
  } yield Metadata(
    tags,
    categoryId,
    license,
    commentsEnabled,
    channelId,
    privacyStatus,
    expiryDate,
    pluto)
}