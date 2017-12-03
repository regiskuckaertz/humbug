package com.gu.contentapi.client.model.v1

sealed trait SponsorshipType extends TEnum
case object SPONSORED extends SponsorshipType
case object FOUNDATION extends SponsorshipType
case object PAID_CONTENT extends SponsorshipType

object SponsorshipType extends TEnumCodec[SponsorshipType] {
  override def encode = {
    case SPONSORED    ⇒ 0
    case FOUNDATION   ⇒ 1
    case PAID_CONTENT ⇒ 2
  }
  override def decode = {
    case 0 ⇒ SPONSORED
    case 1 ⇒ FOUNDATION
    case 2 ⇒ PAID_CONTENT
  }
}