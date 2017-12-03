package com.gu.contentapi.client.model.v1

sealed trait AssetType extends TEnum
case object IMAGE extends AssetType
case object VIDEO extends AssetType
case object AUDIO extends AssetType
case object EMBED extends AssetType
case object TWEET extends AssetType

object AssetType extends TEnumCodec[AssetType] {
  override def encode = {
    case IMAGE ⇒ 0
    case VIDEO ⇒ 1
    case AUDIO ⇒ 2
    case EMBED ⇒ 3
    case TWEET ⇒ 4
  }
  override def decode = {
    case 0 ⇒ IMAGE
    case 1 ⇒ VIDEO
    case 2 ⇒ AUDIO
    case 3 ⇒ EMBED
    case 4 ⇒ TWEET
  }
}