package humbug
package tests

import humbug.codec._
import org.scalacheck._

object ThriftCompactSpecification extends Properties("ThriftCompact") {
  include(BaseSpecification)
  include(ContainerSpecification)
  include(StructSpecification)
}

object BaseSpecification extends Properties("Base Types") {
  import Prop.{forAll, BooleanOperators, all}

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

object ContainerSpecification extends Properties("Container Types") {

}

object StructSpecification extends Properties("Structs") {

}
