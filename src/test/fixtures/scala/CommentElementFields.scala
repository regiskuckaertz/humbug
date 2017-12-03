package com.gu.contentapi.client.model.v1

case class CommentElementFields(source: Option[String]= None,discussionKey: Option[String]= None,commentUrl: Option[String]= None,originalUrl: Option[String]= None,sourceUrl: Option[String]= None,discussionUrl: Option[String]= None,authorUrl: Option[String]= None,html: Option[String]= None,authorName: Option[String]= None,commentId: Option[Int]= None) extends TStruct

object CommentElementFields extends TStructCodec[CommentElementFields]{
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

implicit val r2 = new TFieldCodec[w2.T,Option[String]]()

implicit val r3 = new TFieldCodec[w3.T,Option[String]]()

implicit val r4 = new TFieldCodec[w4.T,Option[String]]()

implicit val r5 = new TFieldCodec[w5.T,Option[String]]()

implicit val r6 = new TFieldCodec[w6.T,Option[String]]()

implicit val r7 = new TFieldCodec[w7.T,Option[String]]()

implicit val r8 = new TFieldCodec[w8.T,Option[String]]()

implicit val r9 = new TFieldCodec[w9.T,Option[String]]()

implicit val r10 = new TFieldCodec[w10.T,Option[Int]]()

override val defaults = HMap[TFieldCodec](w1.value -> None,w2.value -> None,w3.value -> None,w4.value -> None,w5.value -> None,w6.value -> None,w7.value -> None,w8.value -> None,w9.value -> None,w10.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.source,w2.value -> x.discussionKey,w3.value -> x.commentUrl,w4.value -> x.originalUrl,w5.value -> x.sourceUrl,w6.value -> x.discussionUrl,w7.value -> x.authorUrl,w8.value -> x.html,w9.value -> x.authorName,w10.value -> x.commentId)

override def decode = (m) => for {
source <- m.get(w1.value).orElse(defaults.get(w1.value))

discussionKey <- m.get(w2.value).orElse(defaults.get(w2.value))

commentUrl <- m.get(w3.value).orElse(defaults.get(w3.value))

originalUrl <- m.get(w4.value).orElse(defaults.get(w4.value))

sourceUrl <- m.get(w5.value).orElse(defaults.get(w5.value))

discussionUrl <- m.get(w6.value).orElse(defaults.get(w6.value))

authorUrl <- m.get(w7.value).orElse(defaults.get(w7.value))

html <- m.get(w8.value).orElse(defaults.get(w8.value))

authorName <- m.get(w9.value).orElse(defaults.get(w9.value))

commentId <- m.get(w10.value).orElse(defaults.get(w10.value))
} yield CommentElementFields(source,discussionKey,commentUrl,originalUrl,sourceUrl,discussionUrl,authorUrl,html,authorName,commentId)
}