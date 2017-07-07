package humbug

trait ThriftEnum {
  def value: Int
}

object ThriftEnum {
  def from(i: Int): Option[ThriftEnum] = None
}
