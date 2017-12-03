package com.gu.contententity.thrift.entity.film

case class Film(title: String,year: Short,imdbId: String,directors: List[Person],actors: List[Person],genre: List[String]) extends TStruct

object Film extends TStructCodec[Film]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

val w4 = Witness(4)

val w5 = Witness(5)

val w6 = Witness(6)

implicit val r1 = new TFieldCodec[w1.T,String]()

implicit val r2 = new TFieldCodec[w2.T,Short]()

implicit val r3 = new TFieldCodec[w3.T,String]()

implicit val r4 = new TFieldCodec[w4.T,List[Person]]()

implicit val r5 = new TFieldCodec[w5.T,List[Person]]()

implicit val r6 = new TFieldCodec[w6.T,List[String]]()

override val defaults = HMap[TFieldCodec]()

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.title,w2.value -> x.year,w3.value -> x.imdbId,w4.value -> x.directors,w5.value -> x.actors,w6.value -> x.genre)

override def decode = (m) => for {
title <- m.get(w1.value).orElse(defaults.get(w1.value))

year <- m.get(w2.value).orElse(defaults.get(w2.value))

imdbId <- m.get(w3.value).orElse(defaults.get(w3.value))

directors <- m.get(w4.value).orElse(defaults.get(w4.value))

actors <- m.get(w5.value).orElse(defaults.get(w5.value))

genre <- m.get(w6.value).orElse(defaults.get(w6.value))
} yield Film(title,year,imdbId,directors,actors,genre)
}