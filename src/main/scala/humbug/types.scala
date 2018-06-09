package humbug

trait TEnum

trait TTypeDef extends Any

abstract class TStruct protected {
  def leftovers: Fields = Map.empty
}

abstract class TUnion extends Product with Serializable