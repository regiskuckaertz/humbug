package contentatom.timeline

case class TimelineAtom(typeLabel: Option[String]= None,events: List[TimelineItem],description: Option[String]= None) extends TStruct

object TimelineAtom extends TStructCodec[TimelineAtom]{
val w1 = Witness(1)

val w2 = Witness(3)

val w3 = Witness(4)

implicit val r1 = new TFieldCodec[w1.T,Option[String]]()

implicit val r2 = new TFieldCodec[w2.T,List[TimelineItem]]()

implicit val r3 = new TFieldCodec[w3.T,Option[String]]()

override val defaults = HMap[TFieldCodec](w1.value -> None,w3.value -> None)

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.typeLabel,w2.value -> x.events,w3.value -> x.description)

override def decode = (m) => for {
typeLabel <- m.get(w1.value).orElse(defaults.get(w1.value))

events <- m.get(w2.value).orElse(defaults.get(w2.value))

description <- m.get(w3.value).orElse(defaults.get(w3.value))
} yield TimelineAtom(typeLabel,events,description)
}