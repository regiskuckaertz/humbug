package humbug
package codec

trait ThriftCodec[T] {
  def read(d: ThriftReader[T]) = d.read
  def write(c: ThriftWriter[T]) = c.write
}
