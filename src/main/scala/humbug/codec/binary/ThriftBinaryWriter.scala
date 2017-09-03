package humbug
package codec
package binary

import shapeless._, shapeless.labelled._, shapeless.syntax.singleton._

trait ThriftBinaryWriter[T] extends ThriftWriter[T]

object ThriftBinaryWriter
  extends ThriftBinaryBaseWriter
  with ThriftBinaryContainerWriter
  with ThriftBinaryStructWriter
  with ThriftBinaryMessageWriter

trait ThriftBinaryBaseWriter {
  implicit val i8Writer = new ThriftBinaryWriter[Byte] {
    def write = _ #:: Stream.Empty
  }

  implicit val i16Writer = new ThriftBinaryWriter[Short] {
    def write = s => ((s >>> 8) & 0xFF).toByte #:: (s & 0xFF).toByte #:: Stream.Empty
  }

  implicit val i32Writer = new ThriftBinaryWriter[Int] {
    private def serialize: Int => Stream[Byte] = x =>
      (0 until 4).foldLeft(Stream.Empty: Stream[Byte]) { (res: Stream[Byte], i: Int) =>
        ((x >>> i * 8) & 0xFF).toByte #:: res
    }
    def write = serialize
  }

  implicit val i64Writer = new ThriftBinaryWriter[Long] {
    private def serialize: Long => Stream[Byte] = x =>
      (0 until 8).foldLeft(Stream.Empty: Stream[Byte]) { (res: Stream[Byte], i: Int) =>
        ((x >>> i * 8) & 0xFF).toByte #:: res
    }
    def write = serialize
  }

  // Enums: The generated code writes Enums by taking the ordinal
  // value and then encoding that as an int32.
  implicit def enumWriter[A <: ThriftEnum](
    implicit codec: ThriftEnumGeneric[A]
  ) = new ThriftBinaryWriter[A] {
    def write = codec.to andThen i32Writer.write
  }

  // Binary is sent as follows: byte length ++ bytes, where
  // - byte length is the length of the byte array, using
  //   var int encoding (must be >= 0).
  // - bytes are the bytes of the byte array.
  implicit val binaryWriter = new ThriftBinaryWriter[Vector[Byte]] {
    def write = bs => i32Writer.write(bs.length) #::: bs.toStream
  }

  // Values of type double are first converted to an int64 according to the
  // IEEE 754 floating-point "double format" bit layout.
  implicit val doubleWriter = new ThriftBinaryWriter[Double] {
    def write = java.lang.Double.doubleToLongBits _ andThen i64Writer.write
  }

  // Strings are first writed to UTF-8, and then sent as binary.
  implicit val stringWriter = new ThriftBinaryWriter[String] {
    def write = s => binaryWriter.write(s.getBytes("UTF-8").toVector)
  }

  // Element values of type bool are sent as an int8; true as 1 and false as 0.
  implicit val booleanWriter = new ThriftBinaryWriter[Boolean] {
    private val f: Stream[Byte] = (0: Byte) #:: Stream.Empty
    private val t: Stream[Byte] = (1: Byte) #:: Stream.Empty
    def write = {
      case false => f
      case true => t
    }
  }
}

trait ThriftBinaryContainerWriter {
  private def writeListHeader[A, F[_] <: Iterable[_]](l: F[A], et: Int)(
    implicit
    i32: ThriftBinaryWriter[Int]
  ): Stream[Byte] =
    et.toByte #:: i32.write(l.size)

  private def writeListElements[A](l: List[A])(
    implicit
    enc: ThriftBinaryWriter[A]
  ): Stream[Byte] =
    l.foldLeft(Stream.Empty: Stream[Byte])((r: Stream[Byte], e: A) => enc.write(e) #::: r)

  private def writeSetElements[A](l: Set[A])(
    implicit
    enc: ThriftBinaryWriter[A]
  ): Stream[Byte] =
    l.foldLeft(Stream.Empty: Stream[Byte])((r: Stream[Byte], e: A) => enc.write(e) #::: r)

  private def writeMapHeader(m: Map[_, _], kt: Int, vt: Int)(
    implicit
    i32: ThriftBinaryWriter[Int]
  ): Stream[Byte] =
    kt.toByte #:: vt.toByte #:: i32.write(m.size)

  private def writeMapPairs[K, V](m: Map[K, V])(
    implicit
    kenc: ThriftBinaryWriter[K],
    venc: ThriftBinaryWriter[V]): Stream[Byte] =
    m.foldLeft(Stream.Empty: Stream[Byte]) {
      case (r, (k, v)) =>
        kenc.write(k) #::: venc.write(v) #::: r
    }

  implicit def listWriter[A : ThriftBinaryWriter](
    implicit
    w: BinaryWitness[A]
  ) = new ThriftBinaryWriter[List[A]] {
    def write = l => writeListHeader(l, w.value) #::: writeListElements(l)
  }

  implicit def setWriter[A : ThriftBinaryWriter](
    implicit
    w: BinaryWitness[A]
  ) = new ThriftBinaryWriter[Set[A]] {
    def write = s => writeListHeader(s, w.value) #::: writeSetElements(s)
  }

  implicit def mapWriter[K : ThriftBinaryWriter, V : ThriftBinaryWriter](
    implicit
    kw: BinaryWitness[K],
    vw: BinaryWitness[V]
  ) = new ThriftBinaryWriter[Map[K, V]] {
    def write = m => writeMapHeader(m, kw.value, vw.value) #::: writeMapPairs(m)
  }
}

trait ThriftBinaryStructWriter {
  import internal._

  private def writeFieldHeader(fid: Int, tid: Int)(
    implicit
    intw: ThriftBinaryWriter[Int]
  ): Stream[Byte] =
    tid.toByte #:: intw.write(fid)

  private implicit val hnilWriter = new ThriftBinaryWriter[HNil] {
    def write = h => (0: Byte) #:: Stream.Empty
  }

  private implicit def hconsWriter[H, K <: Int, T <: HList](
    implicit
    he: Lazy[ThriftBinaryWriter[H]],
    hw: BinaryWitness[H],
    kw: Witness.Aux[K],
    te: ThriftBinaryWriter[T]
  ) = new ThriftBinaryWriter[FieldType[K, H] :: T] {
    def write = v =>
      writeFieldHeader(kw.value, hw.value) #::: he.value.write(v.head) #::: te.write(v.tail)
  }

  implicit def structWriter[T <: ThriftStruct, H <: HList](
    implicit
    gen: PositionedGeneric.Aux[T, H],
    wri: Lazy[ThriftBinaryWriter[H]]
  ) = new ThriftBinaryWriter[T] {
    def write = gen.to _ andThen wri.value.write
  }
}

trait ThriftBinaryMessageWriter {
  private val version: Short = 0x01.toShort

  private def writeMessageHeader(
    id: Int,
    `type`: ThriftMessageType,
    name: String
  )(
    implicit
    i32: ThriftBinaryWriter[Int],
    str: ThriftBinaryWriter[String]
  ) = {
    val firstByte: Byte = (0x80 | (version >>> 8)).toByte
    val secondByte: Byte = (version & 0xFF).toByte

    firstByte #:: secondByte #:: 0x00.toByte #:: `type`.value.toByte #:: str.write(name) #::: i32.write(id)
  }

  implicit def messageWriter[A](
    implicit
    A: ThriftBinaryWriter[A]
  ) = new ThriftBinaryWriter[ThriftMessage[A]] {
    def write = m =>
      writeMessageHeader(m.id, m.`type`, m.name) #::: A.write(m.value)
  }
}
