package humbug
package codec
package compact

import shapeless._, shapeless.labelled._

trait ThriftCompactReader[T] extends ThriftReader[T]

object ThriftCompactReader
  extends ThriftCompactBaseReader
  with ThriftCompactContainerReader
  with ThriftCompactStructReader
  with ThriftCompactMessageReader

trait ThriftCompactBaseReader {
  // i8 integers
  implicit val i8Writer = new ThriftCompactReader[Byte] {
    def read = {
      case 1 #:: b #:: bs => Some((b, bs))
      case _ => None
    }
  }

  // i16 integers
  implicit val i16Reader = new ThriftCompactReader[Short] {
    def read = i32Reader.read(_) map {
      case (i, s) => (i.toShort, s)
    }
  }

  // i32 integers
  implicit val i32Reader = new ThriftCompactReader[Int] {
    def read = varIntToInt(_) match {
      case (i, s) => Some((zigzagToInt(i), s))
    }
  }

  // i64 integers
  implicit val i64Reader = new ThriftCompactReader[Long] {
    def read = varIntToLong(_) match {
      case (l, s) => Some((zigzagToLong(l), s))
    }
  }

  implicit def enumReader[A <: ThriftEnum](
    implicit codec: ThriftEnumGeneric[A]
  ) = new ThriftCompactReader[A] {
    def read = bs => for {
      (i, s) <- i32Reader.read(bs)
      enum <- codec.from(i)
    } yield (enum, s)
  }

  implicit val doubleReader = new ThriftCompactReader[Double] {
    def read = i64Reader.read(_) map {
      case (l, s) => (java.lang.Double.longBitsToDouble(l), s)
    }
  }

  implicit val binaryReader = new ThriftCompactReader[Vector[Byte]] {
    def read = varIntToInt(_) match {
      case (i, s) => Some((s.take(i).toVector, s.drop(i)))
    }
  }

  implicit val stringReader = new ThriftCompactReader[String] {
    def read = binaryReader.read(_) map {
      case (cs, s) => (new String(cs.toArray, "UTF-8"), s)
    }
  }

  implicit val booleanReader = new ThriftCompactReader[Boolean] {
    def read = _ match {
      case 1 #:: 0 #:: bs => Some((false, bs))
      case 1 #:: 1 #:: bs => Some((true, bs))
      case _              => None
    }
  }
}

trait ThriftCompactContainerReader {
  import compact.CompactWitness

  private def readListHeader[A, F[_] <: Iterable[_]](
    bs: Stream[Byte],
    et: Int
  ): Option[(Int, Stream[Byte])] = {
    val h: Byte = bs.head
    if((h & 0x0F) != et)
      None
    else if((h & 0xF0) != 0xF0)
      Some((h >>> 4, bs.tail))
    else
      varIntToInt(bs.tail) match {
        case (l, bs) => Some((l, bs))
      }
  }

  private def readListElements[A](bs: Stream[Byte], l: Int)(
    implicit
    enc: ThriftCompactReader[A]) = {
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
    enc: ThriftCompactReader[A]
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

  private def readMapHeader(
    bs: Stream[Byte],
    kt: Int,
    vt: Int
  ): Option[(Int, Stream[Byte])] =
    varIntToInt(bs) match {
      case (i, h #:: bs) if h == ((kt << 4) | vt).toByte => Some((i, bs))
      case _ => None
    }

  private def readMapPairs[K, V](n: Int, bs: Stream[Byte])(
    implicit
    KR: ThriftCompactReader[K],
    VR: ThriftCompactReader[V]
  ) = {
    def loop(bs: Stream[Byte], n: Int, acc: Map[K, V]): Option[(Map[K, V], Stream[Byte])] = {
      if(n == 0)
        Some((acc, bs))
      else
        for {
          (k, bs2) <- KR.read(bs)
          (v, bs3) <- VR.read(bs2)
          res <- loop(bs, n - 1, acc + (k -> v))
        } yield res
    }
    loop(bs, n, Map.empty)
  }

  implicit def listReader[A : ThriftCompactReader](
    implicit
    w: CompactWitness[A]
  ) = new ThriftCompactReader[List[A]] {
    def read = bs => for {
      (l, bs) <- readListHeader(bs, w.value)
      res <- readListElements(bs, l)
    } yield res
  }

  implicit def setReader[A : ThriftCompactReader](
    implicit
    w: CompactWitness[A]
  ) = new ThriftCompactReader[Set[A]] {
    def read = bs => for {
      (l, bs) <- readListHeader(bs, w.value)
      res <- readSetElements[A](bs, l)
    } yield res
  }

  implicit def mapReader[K : ThriftCompactReader, V : ThriftCompactReader](
    implicit
    wk: CompactWitness[K],
    wv: CompactWitness[V]
  ) =
    new ThriftCompactReader[Map[K, V]] {
      def read = {
        case 0 #:: bs => Some((Map.empty, bs))
        case bs => for {
          (n, bs2) <- readMapHeader(bs, wk.value, wv.value)
          res <- readMapPairs[K, V](n, bs2)
        } yield res
      }
    }
}

trait ThriftCompactStructReader {
  import internal.PositionedGeneric, compact.StructWitness

  private def readFieldHeader(bs: Stream[Byte], pid: Int)(
    implicit
    intr: ThriftCompactReader[Int]
  ): Option[(Int, Int, Stream[Byte])] = {
    val h: Byte = bs.head
    if((h & 0x0F).toByte == 0)
      intr.read(bs.tail) map {
        case (cid, bs) => (h, cid, bs)
      }
    else
      Some((h & 0x0F, (h >>> 4) + pid, bs.tail))
  }

  private trait DeltaReader[H <: HList] {
    def read(bs: Stream[Byte], pid: Int): Option[(H, Stream[Byte])]
  }

  private implicit val hnilReader = new DeltaReader[HNil] {
    def read(bs: Stream[Byte], pid: Int) = bs match {
      case 0 #:: bs => Some((HNil, bs))
    }
  }

  private implicit def hconsReader[H, K <: Int, T <: HList](
    implicit
    he: Lazy[ThriftCompactReader[H]],
    hw: StructWitness[H],
    kw: Witness.Aux[K],
    te: DeltaReader[T]
  ) = new DeltaReader[FieldType[K, H] :: T] {
    def read(bs: Stream[Byte], pid: Int) = for {
      (fid, cid, bs2) <- readFieldHeader(bs, pid)
      if fid == hw.value && cid == kw.value
      (v, bs3) <- he.value.read(bs)
      (t, bs4) <- te.read(bs, cid)
    } yield (field[kw.T](v) :: t, bs)
  }

  implicit def structReader[T <: ThriftStruct, H <: HList](
    implicit
    gen: PositionedGeneric.Aux[T, H],
    reader: Lazy[DeltaReader[H]]
  ) = new ThriftCompactReader[T] {
    def read = reader.value.read(_, 0) map {
      case (x, rs) => (gen.from(x), rs)
    }
  }
}

trait ThriftCompactMessageReader {
  private val protocolId: Byte = 0x82.toByte
  private val version: Byte = 0x01.toByte

  private def readMessageHeader(bs: Stream[Byte])(
    implicit
    i32: ThriftCompactReader[Int],
    str: ThriftCompactReader[String]
  ): Option[((Int, ThriftMessageType, String), Stream[Byte])] = bs match {
    case pid #:: h2 #:: bs if pid == protocolId => for {
      mtype <- ThriftMessageType(h2 >>> 5)
      (mid, bs2) <- i32.read(bs)
      (mname, bs4) <- str.read(bs)
    } yield ((mid, mtype, mname), bs)
    case _ => None
  }

  implicit def messageReader[A](
    implicit
    A: ThriftCompactReader[A]
  ) = new ThriftCompactReader[ThriftMessage[A]] {
    def read = bs => for {
      ((mid, mtype, mname), bs) <- readMessageHeader(bs)
      (a, bs) <- A.read(bs)
    } yield (ThriftMessage(mid, mtype, mname, a), bs)
  }
}
