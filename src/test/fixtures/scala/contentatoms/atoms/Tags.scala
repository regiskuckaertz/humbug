package contentatom.recipe

case class Tags(cuisine: List[String],category: List[String],celebration: List[String],dietary: List[String]) extends TStruct

object Tags extends TStructCodec[Tags]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

val w4 = Witness(4)

implicit val r1 = new TFieldCodec[w1.T,List[String]]()

implicit val r2 = new TFieldCodec[w2.T,List[String]]()

implicit val r3 = new TFieldCodec[w3.T,List[String]]()

implicit val r4 = new TFieldCodec[w4.T,List[String]]()

override val defaults = HMap[TFieldCodec]()

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.cuisine,w2.value -> x.category,w3.value -> x.celebration,w4.value -> x.dietary)

override def decode = (m) => for {
cuisine <- m.get(w1.value).orElse(defaults.get(w1.value))

category <- m.get(w2.value).orElse(defaults.get(w2.value))

celebration <- m.get(w3.value).orElse(defaults.get(w3.value))

dietary <- m.get(w4.value).orElse(defaults.get(w4.value))
} yield Tags(cuisine,category,celebration,dietary)
}