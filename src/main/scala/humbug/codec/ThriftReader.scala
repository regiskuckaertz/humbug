package humbug
package codec

trait ThriftReader[A] {
  def read: Stream[Byte] ⇒ Option[(A, Stream[Byte])]
}
