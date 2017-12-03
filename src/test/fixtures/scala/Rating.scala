package contentatom.review

case class Rating(maxRating: Short,actualRating: Short,minRating: Short) extends TStruct

object Rating extends TStructCodec[Rating]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

implicit val r1 = new TFieldCodec[w1.T,Short]()

implicit val r2 = new TFieldCodec[w2.T,Short]()

implicit val r3 = new TFieldCodec[w3.T,Short]()

override val defaults = HMap[TFieldCodec]()

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.maxRating,w2.value -> x.actualRating,w3.value -> x.minRating)

override def decode = (m) => for {
maxRating <- m.get(w1.value).orElse(defaults.get(w1.value))

actualRating <- m.get(w2.value).orElse(defaults.get(w2.value))

minRating <- m.get(w3.value).orElse(defaults.get(w3.value))
} yield Rating(maxRating,actualRating,minRating)
}