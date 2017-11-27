package humbug
package codecs

import scodec.{ Attempt, codecs ⇒ C, Codec, Err }
import scodec.bits.ByteVector

package object binary {
  import TFieldType._

  implicit val int8 = C.int8
  implicit val int16 = C.int16
  implicit val int32 = C.int32
  implicit val int64 = C.int64

  implicit def enum[E <: TEnum](implicit E: TEnumCodec[E]): Codec[E] =
    int32.exmap(
      { i ⇒ Attempt.fromOption(E.decode.lift(i), Err("Unable to decode enum")) },
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
}