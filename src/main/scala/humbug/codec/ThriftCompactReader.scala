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
    def read(bs: Stream[Byte]): (Option[Byte], Stream[Byte]) = bs.head match {
      case 1 => (Some(bs.tail.head), bs.tail.tail)
    }
  }

  // i16 integers
  implicit val i16Reader = new ThriftCompactReader[Short] {
    def read(bs: Stream[Byte]): (Option[Short], Stream[Byte]) = i32Reader.read(bs) match {
      case (Some(i), s) => (Some(i.toShort), s)
    }
  }

  // i32 integers
  implicit val i32Reader = new ThriftCompactReader[Int] {
    def read(bs: Stream[Byte]): (Option[Int], Stream[Byte]) = varIntToInt(bs) match {
      case (i, s) => (Some(zigzagToInt(i)), s)
    }
  }

  // i64 integers
  implicit val i64Reader = new ThriftCompactReader[Long] {
    def read(bs: Stream[Byte]): (Option[Long], Stream[Byte]) = varIntToLong(bs) match {
      case (l, s) => (Some(zigzagToLong(l)), s)
    }
  }

  implicit def enumReader[A <: ThriftEnum : ThriftEnumReader] = new ThriftCompactReader[A] {
    def read(bs: Stream[Byte]): (Option[A], Stream[Byte]) = i32Reader.read(bs) match {
      case (Some(i), s) => (implicitly[ThriftEnumReader[A]].from(i), s)
    }
  }

  implicit val doubleReader = new ThriftCompactReader[Double] {
    def read(bs: Stream[Byte]): (Option[Double], Stream[Byte]) = i64Reader.read(bs) match {
      case (Some(l), s) => (Some(java.lang.Double.longBitsToDouble(l)), s)
    }
  }

  implicit val binaryReader = new ThriftCompactReader[Vector[Byte]] {
    def read(bs: Stream[Byte]): (Option[Vector[Byte]], Stream[Byte]) = varIntToInt(bs) match {
      case (i, s) => (Some(s.take(i).toVector), s.drop(i))
    }
  }

  implicit val stringReader = new ThriftCompactReader[String] {
    def read(bs: Stream[Byte]): (Option[String], Stream[Byte]) = binaryReader.read(bs) match {
      case (Some(cs), s) => (Some(new String(cs.toArray, "UTF-8")), s)
    }
  }

  implicit val booleanReader = new ThriftCompactReader[Boolean] {
    def read(bs: Stream[Byte]): (Option[Boolean], Stream[Byte]) = bs match {
      case 1 #:: 0 #:: rs => (Some(false), rs)
      case 1 #:: 1 #:: rs => (Some(true), rs)
      case _              => (None, bs)
    }
  }
}

trait ThriftCompactContainerReader {
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

    readListElements_rec(bs, l, Nil) match {
      case (Some(xs), bs) => (Some(xs.reverse), bs)
    }
  }

  private def readSetElements[A](bs: Stream[Byte], l: Int)(
    implicit
    enc: ThriftCompactReader[A]): (Option[Set[A]], Stream[Byte]) = {
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
    def read(bs: Stream[Byte]): (Option[List[A]], Stream[Byte]) =
      readListHeader(bs, w.value) match {
        case (Some(l), bs) => readListElements(bs, l)
        case _             => (None, bs)
      }
  }

  implicit def setReader[A : ThriftCompactReader](
    implicit
    w: ContainerWitness[A]
  ) = new ThriftCompactReader[Set[A]] {
    def read(bs: Stream[Byte]): (Option[Set[A]], Stream[Byte]) =
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
      def read(bs: Stream[Byte]): (Option[Map[K, V]], Stream[Byte]) = bs match {
        case 0 #:: Stream.Empty => (Some(Map.empty), Stream.Empty)
        case _ => readMapHeader(bs, wk.value, wv.value) match {
          case (Some(n), bs) => readMapPairs[K, V](n, bs)
          case _             => (None, bs)
        }
      }
    }
}

trait ThriftCompactStructReader {
  val stopField: Byte = 0

  implicit def structReader[T <: ThriftStruct, R <: HList](
    implicit
    gen: LabelledGeneric.Aux[T, R],
    reader: Lazy[ThriftCompactReader[R]]
  ) = new ThriftCompactReader[T] {
    def read(bs: Stream[Byte]): (Option[T], Stream[Byte]) = reader.value.read(bs) match {
      case (Some(x), rs) => (Some(gen.from(x)), rs)
    }
  }
}
