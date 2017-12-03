package com.gu.contentapi.client.model.v1

case class Package(
  packageId:    String,
  articles:     List[PackageArticle],
  packageName:  String,
  lastModified: CapiDateTime) extends TStruct

object Package extends TStructCodec[Package] {
  val w1 = Witness(1)
  val w2 = Witness(2)
  val w3 = Witness(3)
  val w4 = Witness(4)
  implicit val r1 = new TFieldCodec[w1.T, String]
  implicit val r2 = new TFieldCodec[w2.T, List[PackageArticle]]
  implicit val r3 = new TFieldCodec[w3.T, String]
  implicit val r4 = new TFieldCodec[w4.T, CapiDateTime]
  override val defaults = HMap[TFieldCodec]
  override def encode = (x) ⇒ HMap[TFieldCodec](
    w1.value -> x.packageId,
    w2.value -> x.articles,
    w3.value -> x.packageName,
    w4.value -> x.lastModified)
  override def decode = (m) ⇒ for {
    packageId ← m.get(w1.value).orElse(defaults.get(w1.value))
    articles ← m.get(w2.value).orElse(defaults.get(w2.value))
    packageName ← m.get(w3.value).orElse(defaults.get(w3.value))
    lastModified ← m.get(w4.value).orElse(defaults.get(w4.value))
  } yield Package(
    packageId,
    articles,
    packageName,
    lastModified)
}