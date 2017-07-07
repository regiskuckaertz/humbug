package humbug
package codec

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{typeOf, TypeTag}
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
  implicit val i8Writer = new ThriftCompactWriter[Byte] {
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

  implicit val enumReader = new ThriftCompactReader[ThriftEnum] {
    def read(bs: Stream[Byte]): (Option[ThriftEnum], Stream[Byte]) = i32Reader.read(bs) match {
      case (Some(i), s) => (ThriftEnum.from(i), s)
    }
  }

  implicit val doubleReader = new ThriftCompactReader[Double] {
    def read(bs: Stream[Byte]): (Option[Double], Stream[Byte]) = i64Reader.read(bs) match {
      case (Some(l), s) => (Some(java.lang.Double.longBitsToDouble(l)), s)
    }
  }

  implicit val binaryReader = new ThriftCompactReader[Array[Byte]] {
    def read(bs: Stream[Byte]): (Option[Array[Byte]], Stream[Byte]) = i32Reader.read(bs) match {
      case (Some(i), s) => (Some(s.take(i).toArray), s.drop(i))
    }
  }

  implicit val stringReader = new ThriftCompactReader[String] {
    def read(bs: Stream[Byte]): (Option[String], Stream[Byte]) = binaryReader.read(bs) match {
      case (Some(cs), s) => (Some(new String(cs, "UTF-8")), s)
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
  implicit val listReader = new ThriftCompactReader[List[_]] {
    def read(bs: Stream[Byte]): (Option[List[_]], Stream[Byte]) =
      (None, bs)
  }

  val emptyMap: Stream[Byte] = (0: Byte) #:: Stream.empty

  implicit val mapReader = new ThriftCompactReader[Map[_, _]] {
    def read(bs: Stream[Byte]): (Option[Map[_, _]], Stream[Byte]) =
      (None, bs)
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
