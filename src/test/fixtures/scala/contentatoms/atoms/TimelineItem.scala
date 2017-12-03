package contentatom.timeline

case class TimelineItem(
  title:      String,
  date:       DateTime,
  body:       Option[String]       = None,
  entities:   Option[List[Entity]] = None,
  dateFormat: Option[String]       = None,
  toDate:     Option[DateTime]     = None) extends TStruct

object TimelineItem extends TStructCodec[TimelineItem] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  val w5 = Witness(5)
  val w6 = Witness(6)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, DateTime]
  implicit val r3 = new TFieldCodec[w3.T, Option[String]]
  implicit val r4 = new TFieldCodec[w4.T, Option[List[Entity]]]
  implicit val r5 = new TFieldCodec[w5.T, Option[String]]
  implicit val r6 = new TFieldCodec[w6.T, Option[DateTime]]
  override val defaults = HMap[TFieldCodec](
    w3.value -> None,
    w4.value -> None,
    w5.value -> None,
    w6.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.title,
    w2.value -> x.date,
    w3.value -> x.body,
    w4.value -> x.entities,
    w5.value -> x.dateFormat,
    w6.value -> x.toDate)
  override def decode = (m) ⇒ for {
    title ← m.get(w1.value).orElse(defaults.get(w1.value))
    date ← m.get(w2.value).orElse(defaults.get(w2.value))
    body ← m.get(w3.value).orElse(defaults.get(w3.value))
    entities ← m.get(w4.value).orElse(defaults.get(w4.value))
    dateFormat ← m.get(w5.value).orElse(defaults.get(w5.value))
    toDate ← m.get(w6.value).orElse(defaults.get(w6.value))
  } yield TimelineItem(
    title,
    date,
    body,
    entities,
    dateFormat,
    toDate)
}