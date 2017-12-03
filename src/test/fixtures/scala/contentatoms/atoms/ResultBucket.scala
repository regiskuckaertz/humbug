package contentatom.quiz

case class ResultBucket(
  assets:      Option[List[Asset]] = None,
  description: String,
  title:       String,
  share:       String,
  id:          String) extends TStruct

object ResultBucket extends TStructCodec[ResultBucket] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  val w5 = Witness(5)
  implicit val r1 = new TFieldCodec[w1.T, Option[List[Asset]]]
  implicit val r2 = new TFieldCodec[w2.T, String]
  implicit val r3 = new TFieldCodec[w3.T, String]
  implicit val r4 = new TFieldCodec[w4.T, String]
  implicit val r5 = new TFieldCodec[w5.T, String]
  override val defaults = HMap[TFieldCodec](w1.value -> None)
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.assets,
    w2.value -> x.description,
    w3.value -> x.title,
    w4.value -> x.share,
    w5.value -> x.id)
  override def decode = (m) ⇒ for {
    assets ← m.get(w1.value).orElse(defaults.get(w1.value))
    description ← m.get(w2.value).orElse(defaults.get(w2.value))
    title ← m.get(w3.value).orElse(defaults.get(w3.value))
    share ← m.get(w4.value).orElse(defaults.get(w4.value))
    id ← m.get(w5.value).orElse(defaults.get(w5.value))
  } yield ResultBucket(
    assets,
    description,
    title,
    share,
    id)
}