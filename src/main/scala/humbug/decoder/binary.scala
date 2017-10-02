package humbug
package encoder

import humbug.codec.binary.BinaryWitness
import humbug.meta._

import scodec.{ codecs ⇒ C, Codec, Decoder }
import scodec.stream.{ decode ⇒ D, StreamDecoder }
import scodec.bits.{ BinStringSyntax, BitVector, ByteVector }

import shapeless._
import shapeless.ops.hlist.Length

package object binary {
  val int8: StreamDecoder[Byte] = D.once(C.byte)

  val int16: StreamDecoder[Short] = D.once(C.short16)

  val int32: StreamDecoder[Int] = D.once(C.int32)

  val int64: StreamDecoder[Long] = D.once(C.int64)

  val double: StreamDecoder[Double] =
    int64 map java.lang.Double.longBitsToDouble _

  val vector: StreamDecoder[ByteVector] = int32 flatMap { n ⇒
    D.once(C.bytes(n)).isolateBytes(n.toLong)
  }

  val string: StreamDecoder[String] = vector map { bs ⇒
    new String(bs.toArray, "UTF-8")
  }

  val boolean: StreamDecoder[Boolean] = int8 flatMap {
    case 0 ⇒ D.emit(false)
    case 1 ⇒ D.emit(true)
    case x ⇒ D.fail(new Exception(s"Expected 0 or 1 but got $x"))
  }

  implicit def enum[A <: ThriftEnum](
    implicit
    gen: ThriftEnumGeneric[A]
  ): StreamDecoder[A] =
    int32 map gen.from

  def option[A: Decoder]: StreamDecoder[Option[A]] =
    D.tryOnce(Decoder[A]) match {
      case D.empty ⇒ D.emit(None)
      case res     ⇒ res map { a ⇒ Some(a) }
    }

  def list[A: Codec](
    implicit
    w: BinaryWitness[A]
  ): StreamDecoder[List[A]] = for {
    t ← int8
    as ← if (t != w.value)
      D.fail(new Exception(s"Expected list of elements to be of type $w.value"))
    else
      D.once(C.listOfN[A](C.int32, Codec[A]))
  } yield as

  def set[A: Codec](
    implicit
    w: BinaryWitness[A]
  ): StreamDecoder[Set[A]] = for {
    t ← int8
    as ← if (t != w.value)
      D.fail(new Exception(s"Expected set of elements to be of type $w.value"))
    else
      D.once(C.listOfN[A](C.int32, Codec[A]))
  } yield as.toSet

  def map[K: Codec, V: Codec](
    implicit
    KW: BinaryWitness[K],
    VW: BinaryWitness[V]
  ): StreamDecoder[Map[K, V]] = for {
    kt ← int8
    vt ← int8
    kvs ← if (kt != KW.value || vt != VW.value)
      D.fail(new Exception(s"Expected map of elements to be of type $KW.value -> $VW.value"))
    else
      D.once(C.listOfN[(K, V)](C.int32, Codec[(K, V)]))
  } yield kvs.toMap

  private def field[N <: Nat](f: ThriftField[N], t: Byte, id: Short): StreamDecoder[f.Out] =
    f.decode flatMap {
      case x: Boolean if t != 2       ⇒ D.fail(new Exception(s"Unexpected type for field id $id: $t"))
      case x: Byte if t != 3          ⇒ D.fail(new Exception(s"Unexpected type for field id $id: $t"))
      case x: Double if t != 4        ⇒ D.fail(new Exception(s"Unexpected type for field id $id: $t"))
      case x: Short if t != 6         ⇒ D.fail(new Exception(s"Unexpected type for field id $id: $t"))
      case x: Int if t != 8           ⇒ D.fail(new Exception(s"Unexpected type for field id $id: $t"))
      case x: Long if t != 10         ⇒ D.fail(new Exception(s"Unexpected type for field id $id: $t"))
      case x: String if t != 11       ⇒ D.fail(new Exception(s"Unexpected type for field id $id: $t"))
      case x: ThriftStruct if t != 12 ⇒ D.fail(new Exception(s"Unexpected type for field id $id: $t"))
      case x: Map[_, _] if t != 13    ⇒ D.fail(new Exception(s"Unexpected type for field id $id: $t"))
      case x: Set[_] if t != 14       ⇒ D.fail(new Exception(s"Unexpected type for field id $id: $t"))
      case x: List[_] if t != 15      ⇒ D.fail(new Exception(s"Unexpected type for field id $id: $t"))
      case x                          ⇒ D.emit(x)
    }

  implicit def struct[A <: ThriftStruct](
    implicit
    T: ThriftStructGeneric[A]
  ): StreamDecoder[A] = {
    def walk(record: T.G.Repr, l: Int): StreamDecoder[A] = l match {
      case 0 ⇒ D.peek(int8 flatMap {
        case 0 ⇒ D.emit(T.from(record))
        case _ ⇒ D.fail(new Exception("Expected stop field at end of struct"))
      })
      case n ⇒ for {
        t ← int8
        id ← int16
        m ← T.fieldMap.get(id) match {
          case Some((p, f)) ⇒ field(f, t, id) map { v ⇒ record.updatedAt(p, v) }
          case None         ⇒ D.fail(new Exception(s"Unknown field id: $id"))
        }
        r ← walk(m, n - 1)
      } yield r
    }

    walk(T.default, T.length.toInt)
  }

  private def messageVersion =
    D.once(C.bits(16L)) flatMap { v ⇒
      if (v === bin"1000000000000001")
        D.advance(8)
      else
        D.fail(new Exception(s"Message header should match version number, got $v"))
    }

  private def messageType =
    D.advance(5) ++ D.once(C.bits(3L)) flatMap { t ⇒
      t.toBin match {
        case "001" ⇒ D.emit(ThriftMessageCall)
        case "010" ⇒ D.emit(ThriftMessageReply)
        case "011" ⇒ D.emit(ThriftMessageException)
        case "100" ⇒ D.emit(ThriftMessageOneWay)
        case x     ⇒ D.fail(new Exception(s"Unrecognized message type $x"))
      }
    }

  def message[A: Codec] = for {
    _ ← messageVersion
    `type` ← messageType
    name ← string
    id ← int32
    value ← D.once(Codec[A])
  } yield ThriftMessage(id, `type`, name, value)
}