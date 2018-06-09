package humbug
package codecs

import scodec.{ Attempt, codecs ⇒ C, Codec, Err }
import scodec.bits.ByteVector

package object binary {
  import TFieldType._

  implicit val int8 = C.byte
  implicit val int16 = C.short16
  implicit val int32 = C.int32
  implicit val int64 = C.int64

  implicit def enum[E <: TEnum](implicit E: TEnumCodec[E]): Codec[E] =
    int32.exmap(
      { i ⇒ Attempt.fromOption(E.decode(i), Err(s"Cannot decode $i")) },
      { e ⇒ Attempt.successful(E.encode(e)) }
    )

  implicit val byteVector: Codec[ByteVector] = C.variableSizeBytes(int32, C.bytes)

  implicit val string: Codec[String] = C.variableSizeBytes(int32, C.utf8)

  implicit val double = C.double

  implicit val boolean: Codec[Boolean] =
    int8.exmap(
      {
        case 0 ⇒ Attempt.successful(false)
        case 1 ⇒ Attempt.successful(true)
        case _ ⇒ Attempt.failure(Err("Unable to decode boolean"))
      },
      {
        case false ⇒ Attempt.successful(0)
        case true  ⇒ Attempt.successful(1)
      }
    )

  implicit def list[A: Codec: TFieldType]: Codec[List[A]] =
    C.constant(ByteVector.fromInt(TFieldType[A].value, 1))
      .dropLeft(C.listOfN(int32, Codec[A]))

  implicit def set[A: Codec: TFieldType]: Codec[Set[A]] =
    C.constant(ByteVector.fromInt(TFieldType[A].value, 1))
      .dropLeft(C.listOfN(int32, Codec[A]).xmap(_.toSet, _.toList))

  implicit def map[K: Codec: TFieldType, V: Codec: TFieldType]: Codec[Map[K, V]] =
    C.constant(ByteVector.fromInt(TFieldType[K].value, 1))
      .dropLeft(C.constant(ByteVector.fromInt(TFieldType[V].value, 1)))
      .dropLeft(C.listOfN(int32, Codec[K] ~ Codec[V]))
      .xmap(_.toMap, _.toList)

  implicit def typedef[A <: TTypeDef, R](implicit A: TTypeDefCodec.Aux[A, R], C: Codec[R]): Codec[A] =
    C.xmap(A.decode, A.encode)

  implicit def struct[A <: TStruct](implicit A: TStructCodec[A]): Codec[A] =
    C.list(int16 ~ dynamic).narrow(
      { a ⇒ Attempt.fromOption(A.decode(Map(a: _*)), Err(s"Cannot decode $a")) },
      { a: A ⇒ A.encode(a).toList }
    ).dropRight(stop)

  implicit def union[A <: TUnion](implicit A: TUnionCodec[A]): Codec[A] =
    (int16 ~ dynamic).narrow(
      a ⇒ Attempt.fromOption(A.decode(a._1, a._2), Err(s"Cannot decode $a")),
      A.encode
    ).dropRight(stop)
}