module Humbug.Thrift
  ( Thrift(..)
  , Header(..)
  , NamespaceScope(..)
  , Definition(..)
  , Field(..)
  , FieldID(..)
  , FieldReq(..)
  , Function(..)
  , FunctionType(..)
  , Throws
  , FieldType(..)
  , ConstValue(..)
  , Literal
  , Identifier

  , isConst
  , isCpp
  , isInclude
  , isPackage
  , isService
  ) where

data Thrift = Thrift { headers :: [Header], definitions :: [Definition] } deriving Show

data Header = Include Literal
            | CppInclude Literal
            | Namespace NamespaceScope Identifier
            deriving Show

data NamespaceScope = NsStar 
                    | NsCpp 
                    | NsJava 
                    | NsPython 
                    | NsPerl 
                    | NsRuby
                    | NsCocoa 
                    | NsCsharp 
                    deriving Show

data Definition = Const FieldType Identifier ConstValue
                | Typedef FieldType Identifier
                | Enum Identifier [(Identifier, Maybe ConstValue)]
                | Struct Identifier [Field]
                | Union Identifier [Field]
                | Exception Identifier [Field]
                | Service Identifier (Maybe Identifier) [Function]
                deriving Show

data Field = Field  { fieldId :: (Maybe FieldID)
                    , fieldReq :: (Maybe FieldReq)
                    , fieldType :: FieldType
                    , fieldIdent :: Identifier 
                    , fieldValue :: (Maybe ConstValue)
                    } deriving (Eq, Show)

type FieldID = Int

data FieldReq = Required | Optional deriving (Eq, Show)

data Function = Function Bool FunctionType Identifier [Field] (Maybe Throws) deriving Show

data FunctionType = LtVoid | LtReturn FieldType deriving Show

type Throws = [Field]

data FieldType =  FtBool
                | FtByte
                | FtInt8
                | FtInt16
                | FtInt32
                | FtInt64
                | FtDouble
                | FtString
                | FtBinary
                | FtMap FieldType FieldType
                | FtSet FieldType
                | FtList FieldType
                | FtNamed Identifier
               deriving (Eq, Show)

data ConstValue = CvInt Int
                | CvDouble Double
                | CvLiteral Literal
                | CvNamed Identifier
                | CvList [ConstValue]
                | CvMap [(ConstValue, ConstValue)]
                deriving (Eq, Show)

type Literal = String

type Identifier = String

isPackage, isCpp, isInclude :: Header -> Bool
isPackage (Namespace _ _) = True
isPackage _               = False

isCpp (CppInclude _)      = True
isCpp _                   = False

isInclude (Include _)     = True
isInclude _               = False

isConst :: Definition -> Bool
isConst (Const _ _ _)     = True
isConst _                 = False

isService :: Definition -> Bool
isService (Service _ _ _ )= True
isService _               = False