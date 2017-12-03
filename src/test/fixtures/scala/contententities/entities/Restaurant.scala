package com.gu.contententity.thrift.entity.restaurant

case class Restaurant(restaurantName: String,approximateLocation: Option[String]= None,webAddress: Option[String]= None,address: Option[Address]= None,geolocation: Option[Geolocation]= None) extends TStruct

object Restaurant extends TStructCodec[Restaurant]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

val w4 = Witness(4)

val w5 = Witness(5)

implicit val r1 = new TFieldCodec[w1.T,String]()

implicit val r2 = new TFieldCodec[w2.T,Option[String]]()

implicit val r3 = new TFieldCodec[w3.T,Option[String]]()

implicit val r4 = new TFieldCodec[w4.T,Option[Address]]()

implicit val r5 = new TFieldCodec[w5.T,Option[Geolocation]]()

override val defaults = HMap[TFieldCodec](w2.value -> None,w3.value -> None,w4.value -> None,w5.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.restaurantName,w2.value -> x.approximateLocation,w3.value -> x.webAddress,w4.value -> x.address,w5.value -> x.geolocation)

override def decode = (m) => for {
restaurantName <- m.get(w1.value).orElse(defaults.get(w1.value))

approximateLocation <- m.get(w2.value).orElse(defaults.get(w2.value))

webAddress <- m.get(w3.value).orElse(defaults.get(w3.value))

address <- m.get(w4.value).orElse(defaults.get(w4.value))

geolocation <- m.get(w5.value).orElse(defaults.get(w5.value))
} yield Restaurant(restaurantName,approximateLocation,webAddress,address,geolocation)
}