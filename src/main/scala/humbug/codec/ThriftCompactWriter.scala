package humbug
package codec

import shapeless._, shapeless.labelled._, shapeless.syntax.singleton._

trait ThriftCompactWriter[T] {
  def write(t: T): Stream[Byte]
}

object ThriftCompactWriter
  extends ThriftCompactBaseWriter
  with ThriftCompactContainerWriter
  with ThriftCompactStructWriter

trait ThriftCompactBaseWriter {
  // i8 integers are treated as binary strings of length 1
  implicit val i8Writer = new ThriftCompactWriter[Byte] {
    def write(b: Byte) = (1: Byte) #:: b #:: Stream.Empty
  }

  // i16 integers are coerced into i32s
  implicit val i16Writer = new ThriftCompactWriter[Short] {
    def write(s: Short) = i32Writer.write(s)
  }

  // i32 integers are zigzag'd and then converted to var ints
  implicit val i32Writer = new ThriftCompactWriter[Int] {
    val serialize: Int => Stream[Byte] = intToVarInt _ compose intToZigZag _
    def write(i: Int) = serialize(i)
  }

  // i64 integers are zigzag's and then converted to var ints
  implicit val i64Writer = new ThriftCompactWriter[Long] {
    val serialize: Long => Stream[Byte] = longToVarInt _ compose longToZigZag _
    def write(l: Long) = serialize(l)
  }

  // Enums: The generated code writes Enums by taking the ordinal
  // value and then encoding that as an int32.
  implicit def enumWriter[A <: ThriftEnum] = new ThriftCompactWriter[A] {
    def write(e: A) = i32Writer.write(e.value)
  }

  // Binary is sent as follows: byte length ++ bytes, where
  // - byte length is the length of the byte array, using
  //   var int encoding (must be >= 0).
  // - bytes are the bytes of the byte array.
  implicit val binaryWriter = new ThriftCompactWriter[Vector[Byte]] {
    def write(bs: Vector[Byte]) = intToVarInt(bs.length) #::: bs.toStream
  }

  // Values of type double are first converted to an int64 according to the
  // IEEE 754 floating-point "double format" bit layout.
  implicit val doubleWriter = new ThriftCompactWriter[Double] {
    def write(d: Double) =
      i64Writer.write(java.lang.Double.doubleToLongBits(d))
  }

  // Strings are first writed to UTF-8, and then sent as binary.
  implicit val stringWriter = new ThriftCompactWriter[String] {
    def write(s: String) =
      binaryWriter.write(s.getBytes("UTF-8").toVector)
  }

  // Element values of type bool are sent as an int8; true as 1 and false as 0.
  val f: Stream[Byte] = (1: Byte) #:: (0: Byte) #:: Stream.Empty
  val t: Stream[Byte] = (1: Byte) #:: (1: Byte) #:: Stream.Empty
  implicit val booleanWriter = new ThriftCompactWriter[Boolean] {
    def write(b: Boolean) = b match {
      case false => f
      case true => t
    }
  }
}

trait ThriftCompactContainerWriter {
  import compact.ContainerWitness

  val emptyMap = (0: Byte) #:: Stream.Empty

  private def writeListHeader[A, F[_] <: Iterable[_]](l: F[A], et: Int): Stream[Byte] = {
    val ls: Int = l.size
    if (ls < 15)
      ((ls << 4) | et).toByte #:: Stream.Empty
    else
      (0xF0 | et).toByte #:: intToVarInt(ls)
  }

  private def writeListElements[A](l: List[A])(
    implicit
    enc: ThriftCompactWriter[A]
  ): Stream[Byte] =
    l.foldLeft(Stream.Empty: Stream[Byte])((r: Stream[Byte], e: A) => enc.write(e) #::: r)

  private def writeSetElements[A](l: Set[A])(
    implicit
    enc: ThriftCompactWriter[A]
  ): Stream[Byte] =
    l.foldLeft(Stream.Empty: Stream[Byte])((r: Stream[Byte], e: A) => enc.write(e) #::: r)

  private def writeMapHeader(m: Map[_, _], kt: Int, vt: Int): Stream[Byte] =
    intToVarInt(m.size) :+ ((kt << 4) | vt).toByte

  private def writeMapPairs[K, V](m: Map[K, V])(
    implicit
    kenc: ThriftCompactWriter[K],
    venc: ThriftCompactWriter[V]): Stream[Byte] =
    m.foldLeft(Stream.Empty: Stream[Byte]) {
      case (r, (k, v)) =>
        kenc.write(k) #::: venc.write(v) #::: r
    }

  implicit def listWriter[A : ThriftCompactWriter](
    implicit
    w: ContainerWitness[A]
  ) = new ThriftCompactWriter[List[A]] {
    def write(l: List[A]) =
      writeListHeader(l, w.value) #::: writeListElements(l)
  }

  implicit def setWriter[A : ThriftCompactWriter](
    implicit
    w: ContainerWitness[A]
  ) = new ThriftCompactWriter[Set[A]] {
    def write(s: Set[A]) =
      writeListHeader(s, w.value) #::: writeSetElements(s)
  }

  implicit def mapWriter[K : ThriftCompactWriter, V : ThriftCompactWriter](
    implicit
    kw: ContainerWitness[K],
    vw: ContainerWitness[V]
  ) = new ThriftCompactWriter[Map[K, V]] {
    def write(m: Map[K, V]) =
      if (m.isEmpty) emptyMap else writeMapHeader(m, kw.value, vw.value) #::: writeMapPairs(m)
  }
}

trait ThriftCompactStructWriter {
  import internal.PositionedGeneric, compact.StructWitness

  private def writeFieldHeader(pid: Int, cid: Int, tid: Int)(
    implicit
    intw: ThriftCompactWriter[Int]
  ): Stream[Byte] = {
    val delta: Int = cid - pid
    if((delta & ~0x0F) == 0)
      ((delta << 4) | tid).toByte #:: Stream.Empty
    else
      tid.toByte #:: intw.write(cid)
  }

  private trait DeltaWriter[H <: HList] {
    def write(t: H, pid: Int): Stream[Byte]
  }

  private implicit val hnilWriter = new DeltaWriter[HNil] {
    def write(h: HNil, pid: Int): Stream[Byte] = (0: Byte) #:: Stream.Empty
  }

  private implicit def hconsWriter[H, K <: Int, T <: HList](
    implicit
    he: Lazy[ThriftCompactWriter[H]],
    hw: StructWitness[H],
    kw: Witness.Aux[K],
    te: DeltaWriter[T]
  ) = new DeltaWriter[FieldType[K, H] :: T] {
    def write(v: FieldType[K, H] :: T, pid: Int) =
      writeFieldHeader(pid, kw.value, hw.value) #::: he.value.write(v.head) #::: te.write(v.tail, kw.value)
  }

  implicit def structWriter[T <: ThriftStruct, H <: HList](
    implicit
    gen: PositionedGeneric.Aux[T, H],
    wri: Lazy[DeltaWriter[H]]
  ) = new ThriftCompactWriter[T] {
    def write(s: T) = wri.value.write(gen.to(s), 0)
  }
}
