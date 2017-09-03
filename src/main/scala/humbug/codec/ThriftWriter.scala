package humbug
package codec

trait ThriftWriter[T] {
  def write(t: T): Stream[Byte]
}
