package humbug
package codec

trait ThriftWriter[A] {
  def write: A => Stream[Byte]
}
