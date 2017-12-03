package com.gu.contentatom.thrift

case class User(email: String,firstName: Option[String]= None,lastName: Option[String]= None) extends TStruct

object User extends TStructCodec[User]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

implicit val r1 = new TFieldCodec[w1.T,String]()

implicit val r2 = new TFieldCodec[w2.T,Option[String]]()

implicit val r3 = new TFieldCodec[w3.T,Option[String]]()

override val defaults = HMap[TFieldCodec](w2.value -> None,w3.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.email,w2.value -> x.firstName,w3.value -> x.lastName)

override def decode = (m) => for {
email <- m.get(w1.value).orElse(defaults.get(w1.value))

firstName <- m.get(w2.value).orElse(defaults.get(w2.value))

lastName <- m.get(w3.value).orElse(defaults.get(w3.value))
} yield User(email,firstName,lastName)
}