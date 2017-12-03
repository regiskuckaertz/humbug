package com.gu.contentatom.thrift

case class Atom(id: ContentAtomID,atomType: AtomType,labels: List[String],defaultHtml: String,data: AtomData,contentChangeDetails: ContentChangeDetails,flags: Option[Flags]= None,title: Option[String]= None,commissioningDesks: Option[List[String]]= Some(List())) extends TStruct

object Atom extends TStructCodec[Atom]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

val w4 = Witness(4)

val w5 = Witness(5)

val w6 = Witness(6)

val w7 = Witness(7)

val w8 = Witness(8)

val w9 = Witness(9)

implicit val r1 = new TFieldCodec[w1.T,ContentAtomID]()

implicit val r2 = new TFieldCodec[w2.T,AtomType]()

implicit val r3 = new TFieldCodec[w3.T,List[String]]()

implicit val r4 = new TFieldCodec[w4.T,String]()

implicit val r5 = new TFieldCodec[w5.T,AtomData]()

implicit val r6 = new TFieldCodec[w6.T,ContentChangeDetails]()

implicit val r7 = new TFieldCodec[w7.T,Option[Flags]]()

implicit val r8 = new TFieldCodec[w8.T,Option[String]]()

implicit val r9 = new TFieldCodec[w9.T,Option[List[String]]]()

override val defaults = HMap[TFieldCodec](w7.value -> None,w8.value -> None,w9.value -> Some(List()))

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.id,w2.value -> x.atomType,w3.value -> x.labels,w4.value -> x.defaultHtml,w5.value -> x.data,w6.value -> x.contentChangeDetails,w7.value -> x.flags,w8.value -> x.title,w9.value -> x.commissioningDesks)

override def decode = (m) => for {
id <- m.get(w1.value).orElse(defaults.get(w1.value))

atomType <- m.get(w2.value).orElse(defaults.get(w2.value))

labels <- m.get(w3.value).orElse(defaults.get(w3.value))

defaultHtml <- m.get(w4.value).orElse(defaults.get(w4.value))

data <- m.get(w5.value).orElse(defaults.get(w5.value))

contentChangeDetails <- m.get(w6.value).orElse(defaults.get(w6.value))

flags <- m.get(w7.value).orElse(defaults.get(w7.value))

title <- m.get(w8.value).orElse(defaults.get(w8.value))

commissioningDesks <- m.get(w9.value).orElse(defaults.get(w9.value))
} yield Atom(id,atomType,labels,defaultHtml,data,contentChangeDetails,flags,title,commissioningDesks)
}