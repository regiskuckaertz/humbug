package com.gu.thrift
package tests

import com.gu.thrift_, com.gu.thrift.codec._
import org.scalacheck.Prop.forAll

object ThriftCompactTest {

  val byteProp = forAll { (b: Byte)(implicit R: ThriftCompactReader[Byte], W: ThriftCompactWriter[Byte]) =>
    R.read(W.write(b)) match {
      case (Some(b2), _) => b == b2
    }
  }
}
