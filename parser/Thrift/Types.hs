module Thrift.Types
( Document(..),
  Header(..),
  NamespaceScope(..),
  Definition(..),
  Field(..),
  FieldID(..),
  FieldReq(..),
  Function(..),
  FunctionType(..),
  Throws,
  FieldType(..),
  DefinitionType(..),
  ContainerType(..),
  ConstValue(..),
  Literal,
  Identifier
) where

data Document = Document [Header] [Definition] deriving Show

data Header = Include Literal
            | CppInclude Literal
            | Namespace NamespaceScope Identifier
            | PhpNamespace Literal
            | XsdNamespace Literal
            deriving Show

type NamespaceScope = String

data Definition = Const FieldType Identifier ConstValue
                | Typedef DefinitionType Identifier
                | Enum Identifier [(Identifier, Maybe ConstValue)]
                | Struct Identifier [Field]
                | Union Identifier [Field]
                | Exception Identifier [Field]
                | Service Identifier (Maybe Identifier) [Function]
                deriving Show

data Field = Field (Maybe FieldID) (Maybe FieldReq) FieldType Identifier (Maybe ConstValue) deriving Show

newtype FieldID = FieldID { ident :: Int } deriving Show

data FieldReq = Required | Optional | DefaultReq deriving Show

data Function = Function Bool FunctionType Identifier [Field] (Maybe Throws) deriving Show

data FunctionType = VoidType | ReturnType FieldType deriving Show

type Throws = [Field]

data FieldType = NamedField Identifier
               | BaseField String
               | ContainerField ContainerType
               deriving Show

data DefinitionType = Base String
                    | Container ContainerType
                    deriving Show

data ContainerType = MapType (FieldType, FieldType)
                   | SetType FieldType
                   | ListType FieldType
                   deriving Show

data ConstValue = IntConstant Integer
                | DoubleConstant Double
                | LiteralString Literal
                | NamedConst Identifier
                | ConstList [ConstValue]
                | ConstMap [(ConstValue, ConstValue)]
                deriving Show

type Literal = String

type Identifier = String
