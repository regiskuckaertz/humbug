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
, scalaField
, scalaLambda
, scalaPair
, scalaForC
, scalaForV
, scalaLiteral
, scalaIdent
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
              | StCaseClass Name [Argument] [Name]
              | StCaseObject Name [Argument] Name
              | StCompanionObject Name (Maybe Name) [a]
              | StMethod Name Bool [Argument] (Maybe Type) [a]
              | StCase Value (Maybe Type) [a]
              | StTrait Name [a]
              | StVal Name Bool Bool (Maybe Type) [a]
              | StNew Name Bool [a] [a]
              | StField Name Name [Argument] [a]
              | StLambda [Argument] [a]
              | StPair Value Value (Maybe Type)
              | StForC [a] Value
              | StForV Name Value
              | StLiteral String
              | StIdent Name
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

scalaCaseClass :: Name -> [Argument] -> [Name] -> Stmt
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

scalaVal :: Name -> Bool -> Bool -> Maybe Type -> [Stmt] -> Stmt
scalaVal n o i t = Fix . StVal n o i t

scalaNew :: Name -> Bool -> [Stmt] -> [Stmt] -> Stmt
scalaNew n o as = Fix . StNew n o as

scalaField :: Name -> Name -> [Argument] -> [Stmt] -> Stmt
scalaField n n' as = Fix . StField n n' as

scalaLambda :: [Argument] -> [Stmt] -> Stmt
scalaLambda as = Fix . StLambda as

scalaPair :: Value -> Value -> Maybe Type -> Stmt
scalaPair v v' = Fix . StPair v v'

scalaForC :: [Stmt] -> Value -> Stmt
scalaForC ss = Fix . StForC ss

scalaForV :: Name -> Value -> Stmt
scalaForV n = Fix . StForV n

scalaLiteral :: Show a => a -> Stmt
scalaLiteral = Fix . StLiteral . show

scalaIdent :: Name -> Stmt
scalaIdent = Fix . StIdent