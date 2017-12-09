package com.gu.contentatom.thrift.atom.media

sealed trait AssetType extends TEnum
case object AUDIO extends AssetType
case object VIDEO extends AssetType

object AssetType extends TEnumCodec[AssetType] {
  override def encode = {
    case AUDIO ⇒ 0
    case VIDEO ⇒ 1
  }
  override def decode = {
    case 0 ⇒ AUDIO
    case 1 ⇒ VIDEO
  }
}