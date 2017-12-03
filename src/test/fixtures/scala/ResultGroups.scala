package contentatom.quiz

case class ResultGroups(groups: List[ResultGroup]) extends TStruct

object ResultGroups extends TStructCodec[ResultGroups]{
val w1 = Witness(1)

implicit val r1 = new TFieldCodec[w1.T,List[ResultGroup]]()

override val defaults = HMap[TFieldCodec]()

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.groups)

override def decode = (m) => for {
groups <- m.get(w1.value).orElse(defaults.get(w1.value))
} yield ResultGroups(groups)
}