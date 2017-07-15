package humbug
package tests

import humbug.codec._
import org.scalacheck._

import Prop.forAll

object ThriftCompactSpecification extends Properties("ThriftCompact") {
  include(BaseSpecification)
  include(EnumSpecification)
  include(ContainerSpecification)
  include(StructSpecification)
}

object BaseSpecification extends Properties("Base Types") {
  property("Bytes") = forAll { (x: Byte) =>
    implicit val R = implicitly[ThriftCompactReader[Byte]]
    implicit val W = implicitly[ThriftCompactWriter[Byte]]
    R.read(W.write(x)) match {
      case (Some(y), _) => x == y
      case (None, _) => false
    }
  }

  property("Shorts") = forAll { (x: Short) =>
    implicit val R = implicitly[ThriftCompactReader[Short]]
    implicit val W = implicitly[ThriftCompactWriter[Short]]
    R.read(W.write(x)) match {
      case (Some(y), _) => x == y
      case (None, _) => false
    }
  }

  property("Ints") = forAll { (x: Int) =>
    implicit val R = implicitly[ThriftCompactReader[Int]]
    implicit val W = implicitly[ThriftCompactWriter[Int]]
    val s = W.write(x)
    R.read(s) match {
      case (Some(y), _) => x == y
      case (None, _) => false
    }
  }

  property("Longs") = forAll { (x: Long) =>
    implicit val R = implicitly[ThriftCompactReader[Long]]
    implicit val W = implicitly[ThriftCompactWriter[Long]]
    R.read(W.write(x)) match {
      case (Some(y), _) => x == y
      case (None, _) => false
    }
  }

  property("Doubles") = forAll { (x: Double) =>
    implicit val R = implicitly[ThriftCompactReader[Double]]
    implicit val W = implicitly[ThriftCompactWriter[Double]]
    R.read(W.write(x)) match {
      case (Some(y), _) => x == y
      case (None, _) => false
    }
  }

  property("Strings") = forAll { (x: String) =>
    implicit val R = implicitly[ThriftCompactReader[String]]
    implicit val W = implicitly[ThriftCompactWriter[String]]
    R.read(W.write(x)) match {
      case (Some(y), _) => x == y
      case (None, _) => false
    }
  }

  property("Byte Arrays") = forAll { (x: Vector[Byte]) =>
    implicit val R = implicitly[ThriftCompactReader[Vector[Byte]]]
    implicit val W = implicitly[ThriftCompactWriter[Vector[Byte]]]
    R.read(W.write(x)) match {
      case (Some(y), _) => x.equals(y)
      case (None, _) => false
    }
  }
}

object EnumSpecification extends Properties("Enums") {
  import samples._

  property("Enums") = forAll(Gen.oneOf(0, 1, 2, 3, 4, 5, 6, 7)) { (x: Int) =>
    implicit val TR = implicitly[ThriftEnumReader[ContentType]]
    implicit val R = implicitly[ThriftCompactReader[ContentType]]
    implicit val W = implicitly[ThriftCompactWriter[ContentType]]
    TR.from(x) map { cx: ContentType =>
      R.read(W.write(cx)) match {
        case (Some(cy), _) => cx == cy
        case _ => false
      }
    } getOrElse false
  }
}

object ContainerSpecification extends Properties("Container Types") {
  property("Lists") = forAll { (xs: List[String]) =>
    implicit val R = implicitly[ThriftCompactReader[List[String]]]
    implicit val W = implicitly[ThriftCompactWriter[List[String]]]
    R.read(W.write(xs)) match {
      case (Some(ys), _) => xs == ys
      case (None, _) => false
    }
  }
}

object StructSpecification extends Properties("Structs") {

}
