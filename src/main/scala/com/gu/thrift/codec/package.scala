package com.gu.thrift

package object codec {

  def intToZigZag(n: Int): Int = (n << 1) ^ (n >> 31)

  def longToZigZag(n: Long): Long = (n << 1) ^ (n >> 63)

  def zigzagToInt(n: Int): Int = (n >>> 1) ^ - (n & 1)
  
  def zigzagToLong(n: Long): Long = (n >>> 1) ^ - (n & 1)

  def varIntToInt(bs: Stream[Byte]): (Int, Stream[Byte]) = {
    val i: Int = 1 + (bs indexWhere ((b: Byte) => (b & 0x80).toByte == 0x00))
    val x: Int = (bs take i).foldLeft(0)((r: Int, b: Byte) => (r << 7) + (b & 0x7F))
    (x, bs drop i)
  }

  def varIntToLong(bs: Stream[Byte]): (Long, Stream[Byte]) = {
    val i: Int = 1 + (bs indexWhere ((b: Byte) => (b & 0x80).toByte == 0x00))
    val x: Long = (bs take i).foldLeft(0L)((r: Long, b: Byte) => (r << 7) + (b & 0x7F))
    (x, bs drop i)
  }

  def intToVarInt(n: Int): Stream[Byte] = {
    val m: Int =      if (n < 0x80)       1
                 else if (n < 0x4000)     2
                 else if (n < 0x200000)   3
                 else if (n < 0x10000000) 4
                 else                     5
    (0 until m).reverse.map { i: Int =>
      ((n >> (i * 7) & 0x7F) | (if (i == 0) 0x00 else 0x80)).toByte
    }.toStream
  }

  def longToVarInt(n: Long): Stream[Byte] = {
    val m: Int =       if (n < 0x80L)                1
                  else if (n < 0x4000L)              2
                  else if (n < 0x200000L)            3
                  else if (n < 0x10000000L)          4
                  else if (n < 0x800000000L)         5
                  else if (n < 0x40000000000L)       6
                  else if (n < 0x2000000000000L)     7
                  else if (n < 0x100000000000000L)   8
                  else if (n < 0x8000000000000000L)  9
                  else                               10
    (0 until m).reverse.map { i: Int =>
      ((n >> (i * 7) & 0x7F) | (if (i == 0) 0x00 else 0x80)).toByte
    }.toStream
  }

}
