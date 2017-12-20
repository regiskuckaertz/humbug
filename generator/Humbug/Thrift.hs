{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Humbug.Thrift
( ThriftF(..)
, Thrift
, document
, include
, cppinclude
, namespace
, constant
, typedef
, enum
, struct
, union
, exception
, field
, service
, function
, NamespaceScope(..)
, FieldID
, FieldReq(..)
, FunctionType(..)
, FieldType(..)
, ConstValue(..)
, Literal
, Identifier
) where

import Data.Functor.Foldable
  
data ThriftF a = ThDocument [a] [a]
               | ThInclude Literal
               | ThCppInclude Literal
               | ThNamespace NamespaceScope Identifier
               | ThConst FieldType Identifier ConstValue
               | ThTypedef FieldType Identifier
               | ThEnum Identifier [(Identifier, Maybe ConstValue)]
               | ThStruct Identifier [a]
               | ThUnion Identifier [a]
               | ThException Identifier [a]
               | ThField (Maybe FieldID) (Maybe FieldReq) FieldType Identifier (Maybe ConstValue)
               | ThService Identifier (Maybe Identifier) [a]
               | ThFunction Bool FunctionType Identifier [a] (Maybe [a])
               deriving (Show, Functor, Foldable, Traversable)

type Thrift = Fix ThriftF

document :: [Thrift] -> [Thrift] -> Thrift
document hs ds = Fix $ ThDocument hs ds

include :: Literal -> Thrift
include lit = Fix $ ThInclude lit

cppinclude :: Literal -> Thrift
cppinclude lit = Fix $ ThCppInclude lit

namespace :: NamespaceScope -> Identifier -> Thrift
namespace ns ident = Fix $ ThNamespace ns ident

constant :: FieldType -> Identifier -> ConstValue -> Thrift
constant ft ident cv = Fix $ ThConst ft ident cv

typedef :: FieldType -> Identifier -> Thrift
typedef ft ident = Fix $ ThTypedef ft ident

enum :: Identifier -> [(Identifier, Maybe ConstValue)] -> Thrift
enum ident fs = Fix $ ThEnum ident fs

struct :: Identifier -> [Thrift] -> Thrift
struct ident fs = Fix $ ThStruct ident fs

union :: Identifier -> [Thrift] -> Thrift
union ident fs = Fix $ ThUnion ident fs

exception :: Identifier -> [Thrift] -> Thrift
exception ident fs = Fix $ ThException ident fs

field :: Maybe FieldID -> Maybe FieldReq -> FieldType -> Identifier -> Maybe ConstValue -> Thrift
field fid freq ft ident fv = Fix $ ThField fid freq ft ident fv

service :: Identifier -> Maybe Identifier -> [Thrift] -> Thrift
service ident pident fns = Fix $ ThService ident pident fns

function :: Bool -> FunctionType -> Identifier -> [Thrift] -> Maybe [Thrift] -> Thrift
function ow ft ident fs ex = Fix $ ThFunction ow ft ident fs ex

data NamespaceScope = NsStar 
                    | NsCpp 
                    | NsJava 
                    | NsPython 
                    | NsPerl 
                    | NsRuby
                    | NsCocoa 
                    | NsCsharp 
                    deriving Show

type FieldID = Int

data FieldReq = Required | Optional deriving Show

data FunctionType = LtVoid | LtReturn FieldType deriving Show

data FieldType = FtNamed Identifier
                | FtMap FieldType FieldType
                | FtSet FieldType
                | FtList FieldType
                | FtBool
                | FtByte
                | FtInt8
                | FtInt16
                | FtInt32
                | FtInt64
                | FtDouble
                | FtString
                | FtBinary
                deriving Show

data ConstValue = CvInt Int
                | CvDouble Double
                | CvLiteral Literal
                | CvNamed Identifier
                | CvList [ConstValue]
                | CvMap [(ConstValue, ConstValue)]
                deriving Show

type Literal = String

type Identifier = String
