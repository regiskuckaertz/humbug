{-# LANGUAGE DeriveFunctor #-}

module Humbug.Scala
( Stmt
, StmtF(..)
, Name
, Type
, Value
, Argument
, scalaPackage
, scalaImport
, scalaPackageObject
, scalaSealedTrait
, scalaCaseClass
, scalaCaseObject
, scalaCompanionObject
, scalaMethod
, scalaCase
, scalaTrait
, scalaVal
, scalaNew
, scalaLambda
, scalaPair
, scalaForC
, scalaForV
, scalaLiteral
) where

import Data.Fix

type Name = String

type Type = String

type Value = String

type Argument = (Name, Maybe Type, Maybe Value)

data StmtF a =  StPackage Name
              | StImport Name [(Name, Maybe Name)]
              | StPackageObject Name [a]
              | StSealedTrait Name (Maybe Name) [a]
              | StCaseClass Name [Argument] (Maybe Name)
              | StCaseObject Name [Argument] Name
              | StCompanionObject Name (Maybe Name) [a]
              | StMethod Name Bool [Argument] (Maybe Type) [a]
              | StCase Value (Maybe Type) [a]
              | StTrait Name [a]
              | StVal Name Bool Bool [a]
              | StNew Name Bool [Argument] [a]
              | StLambda [Argument] [a]
              | StPair Value (Value, Maybe Type)
              | StForC [a] Value
              | StForV Name Value
              | StLiteral String
              deriving (Show, Functor)
              
type Stmt = Fix StmtF

scalaPackage :: Name -> Stmt
scalaPackage = Fix . StPackage

scalaImport :: Name -> [(Name, Maybe Name)] -> Stmt
scalaImport n = Fix . StImport n

scalaPackageObject :: Name -> [Stmt] -> Stmt
scalaPackageObject n = Fix . StPackageObject n

scalaSealedTrait :: Name -> Maybe Name -> [Stmt] -> Stmt
scalaSealedTrait n as = Fix . StSealedTrait n as

scalaCaseClass :: Name -> [Argument] -> Maybe Name -> Stmt
scalaCaseClass n as = Fix . StCaseClass n as

scalaCaseObject :: Name -> [Argument] -> Name -> Stmt
scalaCaseObject n as = Fix . StCaseObject n as

scalaCompanionObject :: Name -> Maybe Name -> [Stmt] -> Stmt
scalaCompanionObject n n' = Fix . StCompanionObject n n'

scalaMethod :: Name -> Bool -> [Argument] -> Maybe Type -> [Stmt] -> Stmt
scalaMethod n o as t = Fix . StMethod n o as t

scalaCase :: Value -> (Maybe Type) -> [Stmt] -> Stmt
scalaCase v t = Fix . StCase v t

scalaTrait :: Name -> [Stmt] -> Stmt
scalaTrait n = Fix . StTrait n

scalaVal :: Name -> Bool -> Bool -> [Stmt] -> Stmt
scalaVal n o i = Fix . StVal n o i

scalaNew :: Name -> Bool -> [Argument] -> [Stmt] -> Stmt
scalaNew n o as = Fix . StNew n o as

scalaLambda :: [Argument] -> [Stmt] -> Stmt
scalaLambda as = Fix . StLambda as

scalaPair :: Value -> (Value, Maybe Type) -> Stmt
scalaPair v = Fix . StPair v

scalaForC :: [Stmt] -> Value -> Stmt
scalaForC ss = Fix . StForC ss

scalaForV :: Name -> Value -> Stmt
scalaForV n = Fix . StForV n

scalaLiteral :: Show a => a -> Stmt
scalaLiteral = Fix . StLiteral . show