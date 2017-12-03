package contentatom.quiz

case class ResultBuckets(buckets: List[ResultBucket]) extends TStruct

object ResultBuckets extends TStructCodec[ResultBuckets]{
val w1 = Witness(1)

implicit val r1 = new TFieldCodec[w1.T,List[ResultBucket]]()

override val defaults = HMap[TFieldCodec]()

override def encode = (x) => HMap[TFieldCodec](w1.value -> x.buckets)

override def decode = (m) => for {
buckets <- m.get(w1.value).orElse(defaults.get(w1.value))
} yield ResultBuckets(buckets)
}