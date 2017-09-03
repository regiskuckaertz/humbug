package humbug
package codec
package binary

import shapeless._, shapeless.labelled._

trait ThriftBinaryReader[T] extends ThriftReader[T]

object ThriftBinaryReader
  extends ThriftBinaryBaseReader
  with ThriftBinaryContainerReader
  with ThriftBinaryStructReader
  with ThriftBinaryMessageReader

trait ThriftBinaryBaseReader {
  // i8 integers
  implicit val i8Writer = new ThriftBinaryReader[Byte] {
    def read = {
      case h #:: t => Some(h, t)
      case _ => None
    }
  }

  // i16 integers
  implicit val i16Reader = new ThriftBinaryReader[Short] {
    def read = {
      case hob #:: lob #:: bs => Some((((hob << 8) | lob).toShort), bs)
      case _ => None
    }
  }

  // i32 integers
  implicit val i32Reader = new ThriftBinaryReader[Int] {
    def read = {
      case b1 #:: b2 #:: b3 #:: b4 #:: bs =>
        Some((b1.toInt << 24) | (b2.toInt << 16) | (b3.toInt << 8) | b4, bs)
      case _ => None
    }
  }

  // i64 integers
  implicit val i64Reader = new ThriftBinaryReader[Long] {
    def read = {
      case b1 #:: b2 #:: b3 #:: b4 #:: b5 #:: b6 #:: b7 #:: b8 #:: bs =>
        Some(((b1.toLong << 54) | (b2.toLong << 48) | (b3.toLong << 40) | (b4.toLong << 32)
          | (b5.toLong << 24) | (b6.toLong << 16) | (b7.toLong << 8) | b8, bs))
      case _ => None
    }
  }

  implicit def enumReader[A <: ThriftEnum](
    implicit codec: ThriftEnumGeneric[A]
  ) = new ThriftBinaryReader[A] {
    def read = i32Reader.read(_) flatMap {
      case (i, bs) => codec.from(i) map { (_, bs) }
    }
  }

  implicit val doubleReader = new ThriftBinaryReader[Double] {
    def read = i64Reader.read(_) map {
      case (l, bs) => (java.lang.Double.longBitsToDouble(l), bs)
    }
  }

  implicit val binaryReader = new ThriftBinaryReader[Vector[Byte]] {
    def read = i32Reader.read(_) map {
      case (i, bs) => (bs.take(i).toVector, bs.drop(i))
    }
  }

  implicit val stringReader = new ThriftBinaryReader[String] {
    def read = binaryReader.read(_) map {
      case (cs, bs) => (new String(cs.toArray, "UTF-8"), bs)
    }
  }

  implicit val booleanReader = new ThriftBinaryReader[Boolean] {
    def read = {
      case 0 #:: bs => Some(false, bs)
      case 1 #:: bs => Some(true, bs)
      case _        => None
    }
  }
}

trait ThriftBinaryContainerReader {
  private def readListHeader[A, F[_] <: Iterable[_]](
    bs: Stream[Byte],
    et: Int
  )(
    implicit
    i32: ThriftBinaryReader[Int]
  ): Option[(Int, Stream[Byte])] = bs match {
    case et2 #:: bs if et.toByte == et2 => i32.read(bs)
    case _ => None
  }

  private def readListElements[A](bs: Stream[Byte], l: Int)(
    implicit
    enc: ThriftBinaryReader[A]): Option[(List[A], Stream[Byte])] = {
    def readListElements_rec(bs: Stream[Byte], l: Int, acc: List[A]): Option[(List[A], Stream[Byte])] = {
      if(l == 0) {
        Some((acc, bs))
      } else {
        enc.read(bs) flatMap {
          case (x, bs) => readListElements_rec(bs, l - 1, x :: acc)
        }
      }
    }

    readListElements_rec(bs, l, Nil)
  }

  private def readSetElements[A](bs: Stream[Byte], l: Int)(
    implicit
    enc: ThriftBinaryReader[A]
  ) = {
    def readSetElements_rec(bs: Stream[Byte], l: Int, acc: Set[A]): Option[(Set[A], Stream[Byte])] = {
      if(l == 0)
        Some((acc, bs))
      else
        enc.read(bs) flatMap {
          case (x, bs) => readSetElements_rec(bs, l - 1, acc + x)
        }
    }

    readSetElements_rec(bs, l, Set.empty[A])
  }

  private def readMapHeader(bs: Stream[Byte], kt: Int, vt: Int)(
    implicit
    i32: ThriftBinaryReader[Int]
  ): Option[(Int, Stream[Byte])] = bs match {
    case ku #:: vu #:: bs if kt == ku && vt == vu => i32.read(bs)
    case _ => None
  }

  private def readMapPairs[K, V](n: Int, bs: Stream[Byte])(
    implicit
    KR: ThriftBinaryReader[K],
    VR: ThriftBinaryReader[V]
  ): Option[(Map[K, V], Stream[Byte])] = {
    def loop(bs: Stream[Byte], n: Int, acc: Map[K, V]): Option[(Map[K, V], Stream[Byte])] = {
      if(n == 0)
        Some((acc, bs))
      else
        KR.read(bs) flatMap {
          case (k, bs) => VR.read(bs) flatMap {
            case (v, bs) => loop(bs, n - 1, acc + (k -> v))
          }
        }
    }
    loop(bs, n, Map.empty)
  }

  implicit def listReader[A : ThriftBinaryReader](
    implicit
    w: BinaryWitness[A]
  ) = new ThriftBinaryReader[List[A]] {
    def read = readListHeader(_, w.value) flatMap {
      case (l, bs) => readListElements(bs, l)
    }
  }

  implicit def setReader[A : ThriftBinaryReader](
    implicit
    w: BinaryWitness[A]
  ) = new ThriftBinaryReader[Set[A]] {
    def read = readListHeader(_, w.value) flatMap {
      case (l, bs) => readSetElements(bs, l)
    }
  }

  implicit def mapReader[K : ThriftBinaryReader, V : ThriftBinaryReader](
    implicit
    wk: BinaryWitness[K],
    wv: BinaryWitness[V]
  ) =
    new ThriftBinaryReader[Map[K, V]] {
      def read = readMapHeader(_, wk.value, wv.value) flatMap {
        case (n, bs) => readMapPairs[K, V](n, bs)
      }
    }
}

trait ThriftBinaryStructReader {
  import internal.PositionedGeneric

  private def readFieldHeader(bs: Stream[Byte], pid: Int)(
    implicit
    i32: ThriftBinaryReader[Int]
  ): Option[(Int, Int, Stream[Byte])] = bs match {
    case h #:: t =>
      if((h & 0x0F).toByte == 0)
        i32.read(t) map {
          case (cid, bs) => (h, cid, bs)
        }
      else
        Some((h & 0x0F, (h >>> 4) + pid, t))
    case _ => None
  }

  private trait DeltaReader[H <: HList] {
    def read: (Stream[Byte], Int) => Option[(H, Stream[Byte])]
  }

  private implicit val hnilReader = new DeltaReader[HNil] {
    def read = (bs, pos) => bs match {
      case 0 #:: bs => Some(HNil, bs)
      case _ => None
    }
  }

  private implicit def hconsReader[H, K <: Int, T <: HList](
    implicit
    he: Lazy[ThriftBinaryReader[H]],
    hw: BinaryWitness[H],
    kw: Witness.Aux[K],
    te: DeltaReader[T]
  ) = new DeltaReader[FieldType[K, H] :: T] {
    def read =
      readFieldHeader(_, _) flatMap {
        case (fid, cid, bs) if fid == hw.value && cid == kw.value => he.value.read(bs) flatMap {
          case (v, bs) => te.read(bs, cid) map {
            case (t, bs) => (field[kw.T](v) :: t, bs)
          }
        }
        case _ => None
      }
  }

  implicit def structReader[T <: ThriftStruct, H <: HList](
    implicit
    gen: PositionedGeneric.Aux[T, H],
    reader: Lazy[DeltaReader[H]]
  ) = new ThriftBinaryReader[T] {
    def read = reader.value.read(_, 0) map {
      case (x, rs) => (gen.from(x), rs)
    }
  }
}

trait ThriftBinaryMessageReader {
  private val version: Short = 0x01.toShort

  private def readMessageHeader(bs: Stream[Byte])(
    implicit
    i32: ThriftBinaryReader[Int],
    str: ThriftBinaryReader[String]
  ): Option[((Int, ThriftMessageType, String), Stream[Byte])] = bs match {
    case _ #:: _ #:: _ #:: mtype #:: bs =>
      ThriftMessageType(mtype) flatMap { mtype =>
        str.read(bs) flatMap {
          case (mname, bs) => i32.read(bs) map {
            case (mid, bs) => ((mid, mtype, mname), bs)
          }
        }
      }
    case _ => None
  }

  implicit def messageReader[A](
    implicit
    A: ThriftBinaryReader[A]
  ) = new ThriftBinaryReader[ThriftMessage[A]] {
    def read = readMessageHeader(_) flatMap {
      case ((mid, mtype, mname), bs) => A.read(bs) map {
        case (a, bs) => (ThriftMessage(mid, mtype, mname, a), bs)
      }
    }
  }
}
