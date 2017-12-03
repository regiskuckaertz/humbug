package com.gu.contententity.thrift

sealed trait EntityType extends TEnum
case object PERSON extends EntityType
case object FILM extends EntityType
case object GAME extends EntityType
case object RESTAURANT extends EntityType
case object PLACE extends EntityType
case object ORGANISATION extends EntityType

object EntityType extends TEnumCodec[EntityType] {
  override def encode = {
    case PERSON       ⇒ 0
    case FILM         ⇒ 1
    case GAME         ⇒ 2
    case RESTAURANT   ⇒ 3
    case PLACE        ⇒ 4
    case ORGANISATION ⇒ 5
  }
  override def decode = {
    case 0 ⇒ PERSON
    case 1 ⇒ FILM
    case 2 ⇒ GAME
    case 3 ⇒ RESTAURANT
    case 4 ⇒ PLACE
    case 5 ⇒ ORGANISATION
  }
}