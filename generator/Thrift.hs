module Humbug.Thrift
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
  ContainerType(..),
  ConstValue(..),
  Literal,
  Identifier
) where

data Document = Document [Header] [Definition] deriving Show

data Header = Include Literal
            | CppInclude Literal
            | Namespace NamespaceScope Identifier
            deriving Show

data NamespaceScope = NsStar 
                    | NsCpp 
                    | NsJava 
                    | NsPython 
                    | NsPerl 
                    | NsCocoa 
                    | NsCsharp 
                    deriving show

data Definition = Const FieldType Identifier ConstValue
                | Typedef FieldType Identifier
                | Enum Identifier [(Identifier, Maybe ConstValue)]
                | Struct Identifier [Field]
                | Union Identifier [Field]
                | Exception Identifier [Field]
                | Service Identifier (Maybe Identifier) [Function]
                deriving Show

data Field = Field (Maybe FieldID) (Maybe FieldReq) FieldType Identifier (Maybe ConstValue) deriving Show

type FieldID = Int

data FieldReq = Required | Optional deriving Show

data Function = Function Bool FunctionType Identifier [Field] (Maybe Throws) deriving Show

data FunctionType = LtVoid | LtReturn FieldType deriving Show

type Throws = [Field]

data FieldType = FtNamed Identifier
               | FtBase BaseType
               | FtContainer ContainerType
               deriving Show

data BaseType = BtBool
              | BtByte
              | BtInt8
              | BtInt16
              | BtInt32
              | BtInt64
              | BtDouble
              | BtString
              | BtBinary
              deriving Show

data ContainerType = CtMap (FieldType, FieldType)
                   | CtSet FieldType
                   | CtList FieldType
                   deriving Show

data ConstValue = CvInt Integer
                | CvDouble Double
                | CvLiteral Literal
                | CvNamed Identifier
                | CvList [ConstValue]
                | CvMap [(ConstValue, ConstValue)]
                deriving Show

type Literal = String

type Identifier = String
