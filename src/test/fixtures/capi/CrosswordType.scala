package com.gu.contentapi.client.model.v1

sealed trait CrosswordType extends TEnum
case object QUICK extends CrosswordType
case object CRYPTIC extends CrosswordType
case object QUIPTIC extends CrosswordType
case object SPEEDY extends CrosswordType
case object PRIZE extends CrosswordType
case object EVERYMAN extends CrosswordType
case object DIAN_QUIPTIC_CROSSWORD extends CrosswordType
case object WEEKEND extends CrosswordType

object CrosswordType extends TEnumCodec[CrosswordType] {
  override def encode = {
    case QUICK                  ⇒ 0

    case CRYPTIC                ⇒ 1

    case QUIPTIC                ⇒ 2

    case SPEEDY                 ⇒ 3

    case PRIZE                  ⇒ 4

    case EVERYMAN               ⇒ 5

    case DIAN_QUIPTIC_CROSSWORD ⇒ 6

    case WEEKEND                ⇒ 7
  }

  override def decode = {
    case 0 ⇒ QUICK

    case 1 ⇒ CRYPTIC

    case 2 ⇒ QUIPTIC

    case 3 ⇒ SPEEDY

    case 4 ⇒ PRIZE

    case 5 ⇒ EVERYMAN

    case 6 ⇒ DIAN_QUIPTIC_CROSSWORD

    case 7 ⇒ WEEKEND
  }
}