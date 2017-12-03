package contentatom.media

case class Version(value: Long) extends AnyVal with TTypeDef

object Version extends TTypeDefCodec[Version, Long] {
  override def encode = `_`.value
  override def decode = new Version(`_`)
}