package contentatom.review

sealed trait ReviewType extends TEnum
case object RESTAURANT extends ReviewType
case object GAME extends ReviewType
case object FILM extends ReviewType

object ReviewType extends TEnumCodec[ReviewType]{
override def encode = {
case RESTAURANT => 1

case GAME => 2

case FILM => 3
}

override def decode = {
case 1 => RESTAURANT

case 2 => GAME

case 3 => FILM
}
}