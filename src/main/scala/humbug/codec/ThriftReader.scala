package humbug
package codec

trait ThriftReader[T] {
  def read: Stream[Byte] => Option[(T, Stream[Byte])]
}
