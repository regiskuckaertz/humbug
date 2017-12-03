package contentatom.guide

case class GuideItem(title: Option[String]= None,body: String,entities: Option[List[Entity]]= None) extends TStruct

object GuideItem extends TStructCodec[GuideItem]{
val w1 = Witness(1)

val w2 = Witness(2)

val w3 = Witness(3)

implicit val r1 = new TFieldCodec[w1.T,Option[String]]()

implicit val r2 = new TFieldCodec[w2.T,String]()

implicit val r3 = new TFieldCodec[w3.T,Option[List[Entity]]]()

override val defaults = HMap[TFieldCodec](w1.value -> None,w3.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.title,w2.value -> x.body,w3.value -> x.entities)

override def decode = (m) => for {
title <- m.get(w1.value).orElse(defaults.get(w1.value))

body <- m.get(w2.value).orElse(defaults.get(w2.value))

entities <- m.get(w3.value).orElse(defaults.get(w3.value))
} yield GuideItem(title,body,entities)
}