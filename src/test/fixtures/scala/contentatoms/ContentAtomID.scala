package com.gu.contentatom.thrift

case class ContentAtomID(value: String) extends AnyVal with TTypeDef

object ContentAtomID extends TTypeDefCodec[ContentAtomID, String] {
  override def encode = `_`.value
  override def decode = new ContentAtomID(`_`)
}