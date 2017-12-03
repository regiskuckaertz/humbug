package contentatom.recipe

case class Time(preparation: Option[Short]= None,cooking: Option[Short]= None) extends TStruct

object Time extends TStructCodec[Time]{
val w1 = Witness(1)

val w2 = Witness(2)

implicit val r1 = new TFieldCodec[w1.T,Option[Short]]()

implicit val r2 = new TFieldCodec[w2.T,Option[Short]]()

override val defaults = HMap[TFieldCodec](w1.value -> None,w2.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.preparation,w2.value -> x.cooking)

override def decode = (m) => for {
preparation <- m.get(w1.value).orElse(defaults.get(w1.value))

cooking <- m.get(w2.value).orElse(defaults.get(w2.value))
} yield Time(preparation,cooking)
}