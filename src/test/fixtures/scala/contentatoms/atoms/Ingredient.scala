package contentatom.recipe

case class Ingredient(item: String,comment: Option[String]= None,quantity: Option[Double]= None,quantityRange: Option[Range]= None,unit: Option[String]= None) extends TStruct

object Ingredient extends TStructCodec[Ingredient]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

val w4 = Witness(4)

val w5 = Witness(5)

implicit val r1 = new TFieldCodec[w1.T,String]()

implicit val r2 = new TFieldCodec[w2.T,Option[String]]()

implicit val r3 = new TFieldCodec[w3.T,Option[Double]]()

implicit val r4 = new TFieldCodec[w4.T,Option[Range]]()

implicit val r5 = new TFieldCodec[w5.T,Option[String]]()

override val defaults = HMap[TFieldCodec](w2.value -> None,w3.value -> None,w4.value -> None,w5.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.item,w2.value -> x.comment,w3.value -> x.quantity,w4.value -> x.quantityRange,w5.value -> x.unit)

override def decode = (m) => for {
item <- m.get(w1.value).orElse(defaults.get(w1.value))

comment <- m.get(w2.value).orElse(defaults.get(w2.value))

quantity <- m.get(w3.value).orElse(defaults.get(w3.value))

quantityRange <- m.get(w4.value).orElse(defaults.get(w4.value))

unit <- m.get(w5.value).orElse(defaults.get(w5.value))
} yield Ingredient(item,comment,quantity,quantityRange,unit)
}