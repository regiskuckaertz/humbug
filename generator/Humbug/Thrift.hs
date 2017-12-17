{-# LANGUAGE DeriveFunctor #-}

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
, FieldTypeF(..)
, FieldType
, ftnamed
, ftbase
, ftmap
, ftset
, ftlist
, ftbool
, ftbyte
, ftint8
, ftint16
, ftint32
, ftint64
, ftdouble
, ftstring
, ftbinary
, ConstValueF(..)
, ConstValue
, cvint
, cvdouble
, cvliteral
, cvnamed
, cvlist
, cvmap
, Literal
, Identifier
) where

import Data.Fix
  
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
               deriving (Functor, Show)
               
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

type Thrift = Fix ThriftF

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

data FieldTypeF a = FtNamed Identifier
                  | FtBase a
                  | FtMap a a
                  | FtSet a
                  | FtList a
                  | FtBool
                  | FtByte
                  | FtInt8
                  | FtInt16
                  | FtInt32
                  | FtInt64
                  | FtDouble
                  | FtString
                  | FtBinary
                  deriving (Functor, Show)

type FieldType = Fix FieldTypeF

ftnamed :: Identifier -> FieldType
ftnamed ident = Fix $ FtNamed ident
ftbase :: FieldType -> FieldType
ftbase b = Fix $ FtBase b
ftmap :: FieldType -> FieldType -> FieldType
ftmap k v = Fix $ FtMap k v
ftset :: FieldType -> FieldType
ftset a = Fix $ FtSet a
ftlist :: FieldType -> FieldType
ftlist a = Fix $ FtList a
ftbool :: FieldType
ftbool = Fix $ FtBool
ftbyte :: FieldType
ftbyte = Fix $ FtByte
ftint8 :: FieldType
ftint8 = Fix $ FtInt8
ftint16 :: FieldType
ftint16 = Fix $ FtInt16
ftint32 :: FieldType
ftint32 = Fix $ FtInt32
ftint64 :: FieldType
ftint64 = Fix $ FtInt64
ftdouble :: FieldType
ftdouble = Fix $ FtDouble
ftstring :: FieldType
ftstring = Fix $ FtString
ftbinary :: FieldType
ftbinary = Fix $ FtBinary

data ConstValueF a = CvInt Int
                   | CvDouble Double
                   | CvLiteral Literal
                   | CvNamed Identifier
                   | CvList [a]
                   | CvMap [(a, a)]
                   deriving (Functor, Show)

type ConstValue = Fix ConstValueF

cvint :: Int -> ConstValue
cvint i = Fix $ CvInt i
cvdouble :: Double -> ConstValue
cvdouble d = Fix $ CvDouble d
cvliteral :: Literal -> ConstValue
cvliteral lit = Fix $ CvLiteral lit
cvnamed :: Identifier -> ConstValue
cvnamed ident = Fix $ CvNamed ident
cvlist :: [ConstValue] -> ConstValue
cvlist as = Fix $ CvList as
cvmap :: [(ConstValue, ConstValue)] -> ConstValue
cvmap kvs = Fix $ CvMap kvs

type Literal = String

type Identifier = String
