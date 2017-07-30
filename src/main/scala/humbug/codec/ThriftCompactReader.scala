package humbug
package codec

import scala.annotation.tailrec
import shapeless._, shapeless.labelled._

trait ThriftCompactReader[T] {
  def read(bs: Stream[Byte]): (Option[T], Stream[Byte])
}

object ThriftCompactReader
  extends ThriftCompactBaseReader
  with ThriftCompactContainerReader
  with ThriftCompactStructReader

trait ThriftCompactBaseReader {
  // i8 integers
  implicit val i8Writer = new ThriftCompactReader[Byte] {
    def read(bs: Stream[Byte]) = bs.head match {
      case 1 => (Some(bs.tail.head), bs.tail.tail)
    }
  }

  // i16 integers
  implicit val i16Reader = new ThriftCompactReader[Short] {
    def read(bs: Stream[Byte]) = i32Reader.read(bs) match {
      case (Some(i), s) => (Some(i.toShort), s)
    }
  }

  // i32 integers
  implicit val i32Reader = new ThriftCompactReader[Int] {
    def read(bs: Stream[Byte]) = varIntToInt(bs) match {
      case (i, s) => (Some(zigzagToInt(i)), s)
    }
  }

  // i64 integers
  implicit val i64Reader = new ThriftCompactReader[Long] {
    def read(bs: Stream[Byte]) = varIntToLong(bs) match {
      case (l, s) => (Some(zigzagToLong(l)), s)
    }
  }

  implicit def enumReader[A <: ThriftEnum](
    implicit codec: ThriftEnumGeneric[A]
  ) = new ThriftCompactReader[A] {
    def read(bs: Stream[Byte]) = i32Reader.read(bs) match {
      case (Some(i), s) => (codec.from(i), s)
    }
  }

  implicit val doubleReader = new ThriftCompactReader[Double] {
    def read(bs: Stream[Byte]) = i64Reader.read(bs) match {
      case (Some(l), s) => (Some(java.lang.Double.longBitsToDouble(l)), s)
    }
  }

  implicit val binaryReader = new ThriftCompactReader[Vector[Byte]] {
    def read(bs: Stream[Byte]) = varIntToInt(bs) match {
      case (i, s) => (Some(s.take(i).toVector), s.drop(i))
    }
  }

  implicit val stringReader = new ThriftCompactReader[String] {
    def read(bs: Stream[Byte]) = binaryReader.read(bs) match {
      case (Some(cs), s) => (Some(new String(cs.toArray, "UTF-8")), s)
    }
  }

  implicit val booleanReader = new ThriftCompactReader[Boolean] {
    def read(bs: Stream[Byte]) = bs match {
      case 1 #:: 0 #:: rs => (Some(false), rs)
      case 1 #:: 1 #:: rs => (Some(true), rs)
      case _              => (None, bs)
    }
  }
}

trait ThriftCompactContainerReader {
  import compact.ContainerWitness

  private def readListHeader[A, F[_] <: Iterable[_]](
    bs: Stream[Byte],
    et: Int
  ): (Option[Int], Stream[Byte]) = {
    val h: Byte = bs.head
    if((h & 0x0F) != et)
      (None, bs)
    else if((h & 0xF0) != 0xF0)
      (Some(h >>> 4), bs.tail)
    else
      varIntToInt(bs.tail) match {
        case (l, bs) => (Some(l), bs)
      }
  }

  private def readListElements[A](bs: Stream[Byte], l: Int)(
    implicit
    enc: ThriftCompactReader[A]): (Option[List[A]], Stream[Byte]) = {
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
    enc: ThriftCompactReader[A]) = {
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

  private def readMapHeader(
    bs: Stream[Byte],
    kt: Int,
    vt: Int
  ): (Option[Int], Stream[Byte]) =
    varIntToInt(bs) match {
      case (i, h #:: bs) if h == ((kt << 4) | vt).toByte => (Some(i), bs)
      case _ => (None, bs)
    }

  private def readMapPairs[K, V](n: Int, bs: Stream[Byte])(
    implicit
    KR: ThriftCompactReader[K],
    VR: ThriftCompactReader[V]
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

  implicit def listReader[A : ThriftCompactReader](
    implicit
    w: ContainerWitness[A]
  ) = new ThriftCompactReader[List[A]] {
    def read(bs: Stream[Byte]) =
      readListHeader(bs, w.value) match {
        case (Some(l), bs) => readListElements(bs, l)
        case _             => (None, bs)
      }
  }

  implicit def setReader[A : ThriftCompactReader](
    implicit
    w: ContainerWitness[A]
  ) = new ThriftCompactReader[Set[A]] {
    def read(bs: Stream[Byte]) =
      readListHeader(bs, w.value) match {
        case (Some(l), bs) => readSetElements(bs, l)
        case _             => (None, bs)
      }
  }

  implicit def mapReader[K : ThriftCompactReader, V : ThriftCompactReader](
    implicit
    wk: ContainerWitness[K],
    wv: ContainerWitness[V]
  ) =
    new ThriftCompactReader[Map[K, V]] {
      def read(bs: Stream[Byte]) = bs match {
        case 0 #:: Stream.Empty => (Some(Map.empty), Stream.Empty)
        case _ => readMapHeader(bs, wk.value, wv.value) match {
          case (Some(n), bs) => readMapPairs[K, V](n, bs)
          case _             => (None, bs)
        }
      }
    }
}

trait ThriftCompactStructReader {
  import internal.PositionedGeneric, compact.StructWitness

  private def readFieldHeader(bs: Stream[Byte], pid: Int)(
    implicit
    intr: ThriftCompactReader[Int]
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
    def read(bs: Stream[Byte], pid: Int) = bs match {
      case 0 #:: bs => (Some(HNil), bs)
    }
  }

  private implicit def hconsReader[H, K <: Int, T <: HList](
    implicit
    he: Lazy[ThriftCompactReader[H]],
    hw: StructWitness[H],
    kw: Witness.Aux[K],
    te: DeltaReader[T]
  ) = new DeltaReader[FieldType[K, H] :: T] {
    def read(bs: Stream[Byte], pid: Int) =
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
  ) = new ThriftCompactReader[T] {
    def read(bs: Stream[Byte]) = reader.value.read(bs, 0) match {
      case (Some(x), rs) => (Some(gen.from(x)), rs)
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
  ): (Option[(Int, Int, String)], Stream[Byte]) = bs match {
    case pid #:: h2 #:: bs if pid == protocolId => {
      val mtype = h2 >>> 5
      i32.read(bs) match {
        case (Some(mid), bs) => str.read(bs) match {
          case (Some(mname), bs) => (Some((mid, mtype, mname)), bs)
        }
      }
    }
  }

  implicit def messageReader[M[_] <: ThriftMessage[A], A](
    implicit
    A: ThriftCompactReader[A]
  ) = new ThriftCompactReader[M[A]] {
    def read(bs: Stream[Byte]) = readMessageHeader(bs) match {
      case (Some((mid, mtype, mname)), bs) => A.read(bs) match {
        // TODO: Fix that sh****
        case (Some(a), bs) => (Some(ThriftMessage(mtype, mid, mname, a).asInstanceOf[M[A]]), bs)
      }
    }
  }
}
