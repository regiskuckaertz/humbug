package com.gu.contentatom.thrift

case class ChangeRecord(date: DateTime,user: Option[User]= None) extends TStruct

object ChangeRecord extends TStructCodec[ChangeRecord]{
val w1 = Witness(1)

val w2 = Witness(2)

implicit val r1 = new TFieldCodec[w1.T,DateTime]()

implicit val r2 = new TFieldCodec[w2.T,Option[User]]()

override val defaults = HMap[TFieldCodec](w2.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.date,w2.value -> x.user)

override def decode = (m) => for {
date <- m.get(w1.value).orElse(defaults.get(w1.value))

user <- m.get(w2.value).orElse(defaults.get(w2.value))
} yield ChangeRecord(date,user)
}