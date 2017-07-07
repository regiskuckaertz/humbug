package humbug
package codec

sealed abstract class ContainerWitness[A](val value: Byte)

object ContainerWitness {
  implicit case object BooleanC extends ContainerWitness[Boolean](2)
  implicit case object ByteC    extends ContainerWitness[Byte](3)
  implicit case object DoubleC  extends ContainerWitness[Double](4)
  implicit case object I16C     extends ContainerWitness[Short](6)
  implicit case object I32C     extends ContainerWitness[Int](8)
  implicit case object I64C     extends ContainerWitness[Long](10)
  implicit case object StringC  extends ContainerWitness[String](11)
  implicit case object BinaryC  extends ContainerWitness[Array[Byte]](11)
  implicit case object StructC  extends ContainerWitness[ThriftStruct](12)
  implicit case object MapC     extends ContainerWitness[Map[_, _]](13)
  implicit case object SetC     extends ContainerWitness[Set[_]](14)
  implicit case object ListC    extends ContainerWitness[List[_]](15)
}
