package com.gu.contententity.thrift

case class Address(formattedAddress: Option[String]= None,streetNumber: Option[Short]= None,streetName: Option[String]= None,neighbourhood: Option[String]= None,postTown: Option[String]= None,locality: Option[String]= None,country: Option[String]= None,administrativeAreaLevelOne: Option[String]= None,administrativeAreaLevelTwo: Option[String]= None,postCode: Option[String]= None) extends TStruct

object Address extends TStructCodec[Address]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

val w4 = Witness(4)

val w5 = Witness(5)

val w6 = Witness(6)

val w7 = Witness(7)

val w8 = Witness(8)

val w9 = Witness(9)

val w10 = Witness(1)

implicit val r1 = new TFieldCodec[w1.T,Option[String]]()

implicit val r2 = new TFieldCodec[w2.T,Option[Short]]()

implicit val r3 = new TFieldCodec[w3.T,Option[String]]()

implicit val r4 = new TFieldCodec[w4.T,Option[String]]()

implicit val r5 = new TFieldCodec[w5.T,Option[String]]()

implicit val r6 = new TFieldCodec[w6.T,Option[String]]()

implicit val r7 = new TFieldCodec[w7.T,Option[String]]()

implicit val r8 = new TFieldCodec[w8.T,Option[String]]()

implicit val r9 = new TFieldCodec[w9.T,Option[String]]()

implicit val r10 = new TFieldCodec[w10.T,Option[String]]()

override val defaults = HMap[TFieldCodec](w1.value -> None,w2.value -> None,w3.value -> None,w4.value -> None,w5.value -> None,w6.value -> None,w7.value -> None,w8.value -> None,w9.value -> None,w10.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.formattedAddress,w2.value -> x.streetNumber,w3.value -> x.streetName,w4.value -> x.neighbourhood,w5.value -> x.postTown,w6.value -> x.locality,w7.value -> x.country,w8.value -> x.administrativeAreaLevelOne,w9.value -> x.administrativeAreaLevelTwo,w10.value -> x.postCode)

override def decode = (m) => for {
formattedAddress <- m.get(w1.value).orElse(defaults.get(w1.value))

streetNumber <- m.get(w2.value).orElse(defaults.get(w2.value))

streetName <- m.get(w3.value).orElse(defaults.get(w3.value))

neighbourhood <- m.get(w4.value).orElse(defaults.get(w4.value))

postTown <- m.get(w5.value).orElse(defaults.get(w5.value))

locality <- m.get(w6.value).orElse(defaults.get(w6.value))

country <- m.get(w7.value).orElse(defaults.get(w7.value))

administrativeAreaLevelOne <- m.get(w8.value).orElse(defaults.get(w8.value))

administrativeAreaLevelTwo <- m.get(w9.value).orElse(defaults.get(w9.value))

postCode <- m.get(w10.value).orElse(defaults.get(w10.value))
} yield Address(formattedAddress,streetNumber,streetName,neighbourhood,postTown,locality,country,administrativeAreaLevelOne,administrativeAreaLevelTwo,postCode)
}