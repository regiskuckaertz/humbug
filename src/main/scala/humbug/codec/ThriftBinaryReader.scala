package humbug
package codec

import scala.annotation.tailrec
import shapeless._, shapeless.labelled._

trait ThriftBinaryReader[T] {
  def read(bs: Stream[Byte]): (Option[T], Stream[Byte])
}

object ThriftBinaryReader
  extends ThriftBinaryBaseReader
  with ThriftBinaryContainerReader
  with ThriftBinaryStructReader

trait ThriftBinaryBaseReader {
  // i8 integers
  implicit val i8Writer = new ThriftBinaryReader[Byte] {
    def read(bs: Stream[Byte]): (Option[Byte], Stream[Byte]) = bs match {
      case h #:: t => (Some(h), t)
    }
  }

  // i16 integers
  implicit val i16Reader = new ThriftBinaryReader[Short] {
    def read(bs: Stream[Byte]): (Option[Short], Stream[Byte]) = bs match {
      case hob #:: lob #:: bs => (Some(((hob << 8) | lob).toShort), bs)
    }
  }

  // i32 integers
  implicit val i32Reader = new ThriftBinaryReader[Int] {
    def read(bs: Stream[Byte]): (Option[Int], Stream[Byte]) = bs match {
      case b1 #:: b2 #:: b3 #:: b4 #:: bs =>
        (Some((b1.toInt << 24) | (b2.toInt << 16) | (b3.toInt << 8) | b4), bs)
    }
  }

  // i64 integers
  implicit val i64Reader = new ThriftBinaryReader[Long] {
    def read(bs: Stream[Byte]): (Option[Long], Stream[Byte]) = bs match {
      case b1 #:: b2 #:: b3 #:: b4 #:: b5 #:: b6 #:: b7 #:: b8 #:: bs =>
        (Some((b1.toLong << 54) | (b2.toLong << 48) | (b3.toLong << 40) | (b4.toLong << 32)
          | (b5.toLong << 24) | (b6.toLong << 16) | (b7.toLong << 8) | b8), bs)
    }
  }

  implicit def enumReader[A <: ThriftEnum](
    implicit codec: ThriftEnumGeneric[A]
  ) = new ThriftBinaryReader[A] {
    def read(bs: Stream[Byte]): (Option[A], Stream[Byte]) = i32Reader.read(bs) match {
      case (Some(i), s) => (codec.from(i), s)
    }
  }

  implicit val doubleReader = new ThriftBinaryReader[Double] {
    def read(bs: Stream[Byte]): (Option[Double], Stream[Byte]) = i64Reader.read(bs) match {
      case (Some(l), s) => (Some(java.lang.Double.longBitsToDouble(l)), s)
    }
  }

  implicit val binaryReader = new ThriftBinaryReader[Vector[Byte]] {
    def read(bs: Stream[Byte]): (Option[Vector[Byte]], Stream[Byte]) = i32Reader.read(bs) match {
      case (Some(i), s) => (Some(s.take(i).toVector), s.drop(i))
    }
  }

  implicit val stringReader = new ThriftBinaryReader[String] {
    def read(bs: Stream[Byte]): (Option[String], Stream[Byte]) = binaryReader.read(bs) match {
      case (Some(cs), s) => (Some(new String(cs.toArray, "UTF-8")), s)
    }
  }

  implicit val booleanReader = new ThriftBinaryReader[Boolean] {
    def read(bs: Stream[Byte]): (Option[Boolean], Stream[Byte]) = bs match {
      case 0 #:: rs => (Some(false), rs)
      case 1 #:: rs => (Some(true), rs)
      case _        => (None, bs)
    }
  }
}

trait ThriftBinaryContainerReader {
  import binary.BinaryWitness

  private def readListHeader[A, F[_] <: Iterable[_]](
    bs: Stream[Byte],
    et: Int
  )(
    implicit
    i32: ThriftBinaryReader[Int]
  ): (Option[Int], Stream[Byte]) = bs match {
    case et2 #:: bs if et.toByte == et2 => i32.read(bs)
    case _ => (None, bs)
  }

  private def readListElements[A](bs: Stream[Byte], l: Int)(
    implicit
    enc: ThriftBinaryReader[A]): (Option[List[A]], Stream[Byte]) = {
    @tailrec def readListElements_rec(bs: Stream[Byte], l: Int, acc: List[A]): (Option[List[A]], Stream[Byte]) = {
      if(l == 0) {
        (Some(acc), bs)
      } else {
        enc.read(bs) match {
          case (Some(x), bs) => readListElements_rec(bs, l - 1, x :: acc)
        }
      }
    }

    readListElements_rec(bs, l, Nil)
  }

  private def readSetElements[A](bs: Stream[Byte], l: Int)(
    implicit
    enc: ThriftBinaryReader[A]): (Option[Set[A]], Stream[Byte]) = {
    @tailrec def readSetElements_rec(bs: Stream[Byte], l: Int, acc: Set[A]): (Option[Set[A]], Stream[Byte]) = {
      if(l == 0)
        (Some(acc), bs)
      else
        enc.read(bs) match {
          case (Some(x), bs) => readSetElements_rec(bs, l - 1, acc + x)
        }
    }

    readSetElements_rec(bs, l, Set.empty[A])
  }

  private def readMapHeader(bs: Stream[Byte], kt: Int, vt: Int)(
    implicit
    i32: ThriftBinaryReader[Int]
  ): (Option[Int], Stream[Byte]) = bs match {
    case ku #:: vu #:: bs if kt == ku && vt == vu => i32.read(bs)
  }

  private def readMapPairs[K, V](n: Int, bs: Stream[Byte])(
    implicit
    KR: ThriftBinaryReader[K],
    VR: ThriftBinaryReader[V]
  ): (Option[Map[K, V]], Stream[Byte]) = {
    @tailrec def loop(bs: Stream[Byte], n: Int, acc: Map[K, V]): (Option[Map[K, V]], Stream[Byte]) = {
      if(n == 0)
        (Some(acc), bs)
      else
        KR.read(bs) match {
          case (Some(k), bs) => VR.read(bs) match {
            case (Some(v), bs) => loop(bs, n - 1, acc + (k -> v))
          }
        }
    }
    loop(bs, n, Map.empty)
  }

  implicit def listReader[A : ThriftBinaryReader](
    implicit
    w: BinaryWitness[A]
  ) = new ThriftBinaryReader[List[A]] {
    def read(bs: Stream[Byte]): (Option[List[A]], Stream[Byte]) =
      readListHeader(bs, w.value) match {
        case (Some(l), bs) => readListElements(bs, l)
      }
  }

  implicit def setReader[A : ThriftBinaryReader](
    implicit
    w: BinaryWitness[A]
  ) = new ThriftBinaryReader[Set[A]] {
    def read(bs: Stream[Byte]): (Option[Set[A]], Stream[Byte]) =
      readListHeader(bs, w.value) match {
        case (Some(l), bs) => readSetElements(bs, l)
      }
  }

  implicit def mapReader[K : ThriftBinaryReader, V : ThriftBinaryReader](
    implicit
    wk: BinaryWitness[K],
    wv: BinaryWitness[V]
  ) =
    new ThriftBinaryReader[Map[K, V]] {
      def read(bs: Stream[Byte]): (Option[Map[K, V]], Stream[Byte]) =
        readMapHeader(bs, wk.value, wv.value) match {
          case (Some(n), bs) => readMapPairs[K, V](n, bs)
        }
    }
}

trait ThriftBinaryStructReader {
  import internal.PositionedGeneric, binary.BinaryWitness

  private def readFieldHeader(bs: Stream[Byte], pid: Int)(
    implicit
    intr: ThriftBinaryReader[Int]
  ): (Int, Int, Stream[Byte]) = {
    val h: Byte = bs.head
    if((h & 0x0F).toByte == 0)
      intr.read(bs.tail) match {
        case (Some(cid), bs) => (h, cid, bs)
      }
    else
      (h & 0x0F, (h >>> 4) + pid, bs.tail)
  }

  private trait DeltaReader[H <: HList] {
    def read(bs: Stream[Byte], pid: Int): (Option[H], Stream[Byte])
  }

  private implicit val hnilReader = new DeltaReader[HNil] {
    def read(bs: Stream[Byte], pid: Int): (Option[HNil], Stream[Byte]) = bs match {
      case 0 #:: bs => (Some(HNil), bs)
    }
  }

  private implicit def hconsReader[H, K <: Int, T <: HList](
    implicit
    he: Lazy[ThriftBinaryReader[H]],
    hw: BinaryWitness[H],
    kw: Witness.Aux[K],
    te: DeltaReader[T]
  ) = new DeltaReader[FieldType[K, H] :: T] {
    def read(bs: Stream[Byte], pid: Int): (Option[FieldType[K, H] :: T], Stream[Byte]) =
      readFieldHeader(bs, pid) match {
        case (fid, cid, bs) if fid == hw.value && cid == kw.value => he.value.read(bs) match {
          case (Some(v), bs) => te.read(bs, cid) match {
            case (Some(t), bs) => (Some(field[kw.T](v) :: t), bs)
          }
        }
      }
  }

  implicit def structReader[T <: ThriftStruct, H <: HList](
    implicit
    gen: PositionedGeneric.Aux[T, H],
    reader: Lazy[DeltaReader[H]]
  ) = new ThriftBinaryReader[T] {
    def read(bs: Stream[Byte]): (Option[T], Stream[Byte]) = reader.value.read(bs, 0) match {
      case (Some(x), rs) => (Some(gen.from(x)), rs)
    }
  }
}
