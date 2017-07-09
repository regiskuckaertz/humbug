package humbug

import scala.annotation.tailrec

package object codec {
  def intToZigZag(n: Int): Int = (n << 1) ^ (n >> 31)

  def zigzagToInt(n: Int): Int = (n >>> 1) ^ - (n & 1)

  def varIntToInt(bs: Stream[Byte]): (Int, Stream[Byte]) = {
    @tailrec def varIntToInt_rec(bs: Stream[(Byte, Int)], acc: Int): (Int, Stream[Byte]) =
      bs match {
        case Stream.Empty                        => (acc, Stream.Empty)
        case (b, i) #:: bs if (b & 0x80) != 0x80 => (acc + (b << (7 * i)), bs.unzip._1)
        case (b, i) #:: bs                       => varIntToInt_rec(bs, acc | ((b & 0x7F) << 7 * i))
      }
    varIntToInt_rec(bs.zipWithIndex, 0)
  }

  def intToVarInt(n: Int): Stream[Byte] = {
    @tailrec def intToVarInt_rec(acc: List[Byte], x: Int): List[Byte] =
      if ((x & ~0x7F) == 0)
        x.toByte :: acc
      else
        intToVarInt_rec(((x & 0x7F) | 0x80).toByte :: acc, x >>> 7)
    intToVarInt_rec(Nil, n).reverse.toStream
  }

  def longToZigZag(n: Long): Long = (n << 1) ^ (n >> 63)

  def zigzagToLong(n: Long): Long = (n >>> 1) ^ - (n & 1)

  def varIntToLong(bs: Stream[Byte]): (Long, Stream[Byte]) = {
    @tailrec def varIntToLong_rec(bs: Stream[(Byte, Int)], acc: Long): (Long, Stream[Byte]) =
      bs match {
        case Stream.Empty                        => (acc, Stream.Empty)
        case (b, i) #:: bs if (b & 0x80) != 0x80 => (acc + (b.toLong << (7 * i)), bs.unzip._1)
        case (b, i) #:: bs                       => varIntToLong_rec(bs, acc | ((b & 0x7F).toLong << 7 * i))
      }
    varIntToLong_rec(bs.zipWithIndex, 0L)
  }

  def longToVarInt(n: Long): Stream[Byte] = {
    @tailrec def intToVarLong_rec(acc: List[Byte], x: Long): List[Byte] =
      if ((x & ~0x7FL) == 0)
        x.toByte :: acc
      else
        intToVarLong_rec(((x & 0x7F) | 0x80).toByte :: acc, x >>> 7)
    intToVarLong_rec(Nil, n).reverse.toStream
  }

}
