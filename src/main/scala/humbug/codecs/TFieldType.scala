package humbug
package codecs

import scodec.bits.ByteVector

private[codecs] class TFieldType[A](val value: Int)

object TFieldType {
  def apply[A](implicit F: TFieldType[A]) = F

  implicit val BOOL = new TFieldType[Boolean](2)
  implicit val BYTE = new TFieldType[Byte](3)
  implicit val DOUBLE = new TFieldType[Double](4)
  implicit val I16 = new TFieldType[Short](6)
  implicit val I32 = new TFieldType[Int](8)
  implicit val I64 = new TFieldType[Long](10)
  implicit val STRING = new TFieldType[String](11)
  implicit val BYTEVECTOR = new TFieldType[ByteVector](11)
  implicit val STRUCT = new TFieldType[TStruct](12)
  implicit val MAP = new TFieldType[Map[_, _]](13)
  implicit val SET = new TFieldType[Set[_]](14)
  implicit val LIST = new TFieldType[List[_]](15)
}