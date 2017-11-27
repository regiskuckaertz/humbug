package com.gu.contentapi.client.model.v1

case class SearchResponse(status: String, userTier: String, total: Int, startIndex: Int, pageSize: Int, currentPage: Int, pages: Int, orderBy: String, results: List[Content]) extends TStruct

object SearchResponse extends TStructCodec[SearchResponse] {
  val w1 = Witness(1)

  val w2 = Witness(2)

  val w3 = Witness(3)

  val w4 = Witness(4)

  val w5 = Witness(5)

  val w6 = Witness(6)

  val w7 = Witness(7)

  val w8 = Witness(8)

  val w9 = Witness(9)

  implicit val r1 = new TFieldCodec[w1.T, String]()

  implicit val r2 = new TFieldCodec[w2.T, String]()

  implicit val r3 = new TFieldCodec[w3.T, Int]()

  implicit val r4 = new TFieldCodec[w4.T, Int]()

  implicit val r5 = new TFieldCodec[w5.T, Int]()

  implicit val r6 = new TFieldCodec[w6.T, Int]()

  implicit val r7 = new TFieldCodec[w7.T, Int]()

  implicit val r8 = new TFieldCodec[w8.T, String]()

  implicit val r9 = new TFieldCodec[w9.T, List[Content]]()

  override val defaults = HMap[TFieldCodec]()

  override def encode = (x) ⇒ HMap[TFieldCodec](w1.value -> x.status, w2.value -> x.userTier, w3.value -> x.total, w4.value -> x.startIndex, w5.value -> x.pageSize, w6.value -> x.currentPage, w7.value -> x.pages, w8.value -> x.orderBy, w9.value -> x.results)

  override def decode = (m) ⇒ for {
    status ← m.get(w1.value).orElse(defaults.get(w1.value))

    userTier ← m.get(w2.value).orElse(defaults.get(w2.value))

    total ← m.get(w3.value).orElse(defaults.get(w3.value))

    startIndex ← m.get(w4.value).orElse(defaults.get(w4.value))

    pageSize ← m.get(w5.value).orElse(defaults.get(w5.value))

    currentPage ← m.get(w6.value).orElse(defaults.get(w6.value))

    pages ← m.get(w7.value).orElse(defaults.get(w7.value))

    orderBy ← m.get(w8.value).orElse(defaults.get(w8.value))

    results ← m.get(w9.value).orElse(defaults.get(w9.value))
  } yield SearchResponse(status, userTier, total, startIndex, pageSize, currentPage, pages, orderBy, results)
}