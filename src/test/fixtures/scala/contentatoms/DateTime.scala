package com.gu.contentatom.thrift

case class DateTime(value: Long) extends AnyVal with TTypeDef

object DateTime extends TTypeDefCodec[DateTime, Long] {
  override def encode = `_`.value
  override def decode = new DateTime(`_`)
}