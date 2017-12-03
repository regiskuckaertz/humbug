package com.gu.contentapi.client.model.v1

sealed trait MembershipTier extends TEnum
case object MEMBERS_ONLY extends MembershipTier
case object PAID_MEMBERS_ONLY extends MembershipTier

object MembershipTier extends TEnumCodec[MembershipTier]{
override def encode = {
case MEMBERS_ONLY => 0

case PAID_MEMBERS_ONLY => 1
}

override def decode = {
case 0 => MEMBERS_ONLY

case 1 => PAID_MEMBERS_ONLY
}
}