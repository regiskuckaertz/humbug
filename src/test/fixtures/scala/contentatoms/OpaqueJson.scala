package com.gu.contentatom.thrift

case class OpaqueJson(value: String) extends AnyVal with TTypeDef

object OpaqueJson extends TTypeDefCodec[OpaqueJson, String] {
  override def encode = `_`.value
  override def decode = new OpaqueJson(`_`)
}