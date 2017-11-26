{-# LANGUAGE DeriveFunctor #-}

module Humbug.Scala
( Stmt
, StmtF(..)
, Name
, Type
, Value
, scalaPackage
, scalaImport
, scalaImportPlaceholder
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
, scalaArgument
, scalaFor
, scalaGenerator
, scalaLiteral
, scalaIdent
, scalaSome
) where

import Data.Fix

type Name = String

type Type = String

type Value = String

data StmtF a =  StPackage Name
              | StImportPlaceholder Name
              | StImport Name [(Name, Maybe Name)]
              | StPackageObject Name [a]
              | StSealedTrait Name (Maybe Name) [a]
              | StCaseClass Name [a] [Name]
              | StCaseObject Name [a] Name
              | StCompanionObject Name (Maybe Name) [a]
              | StMethod Name Bool [a] (Maybe Type) [a]
              | StCase Value (Maybe Type) [a]
              | StTrait Name [a]
              | StVal Name Bool Bool (Maybe Type) [a]
              | StNew Name Bool [a] [a]
              | StField a Name [a] [a]
              | StLambda [a] [a]
              | StPair a a
              | StArgument Name (Maybe Type) (Maybe a)
              | StFor [a] [a]
              | StGenerator Name a
              | StLiteral String
              | StIdent Name
              | StSome a
              deriving (Show, Functor)
              
type Stmt = Fix StmtF

scalaPackage :: Name -> Stmt
scalaPackage = Fix . StPackage

scalaImport :: Name -> [(Name, Maybe Name)] -> Stmt
scalaImport n = Fix . StImport n

scalaImportPlaceholder :: Name -> Stmt
scalaImportPlaceholder = Fix . StImportPlaceholder

scalaPackageObject :: Name -> [Stmt] -> Stmt
scalaPackageObject n = Fix . StPackageObject n

scalaSealedTrait :: Name -> Maybe Name -> [Stmt] -> Stmt
scalaSealedTrait n as = Fix . StSealedTrait n as

scalaCaseClass :: Name -> [Stmt] -> [Name] -> Stmt
scalaCaseClass n as = Fix . StCaseClass n as

scalaCaseObject :: Name -> [Stmt] -> Name -> Stmt
scalaCaseObject n as = Fix . StCaseObject n as

scalaCompanionObject :: Name -> Maybe Name -> [Stmt] -> Stmt
scalaCompanionObject n n' = Fix . StCompanionObject n n'

scalaMethod :: Name -> Bool -> [Stmt] -> Maybe Type -> [Stmt] -> Stmt
scalaMethod n o as t = Fix . StMethod n o as t

scalaCase :: Value -> (Maybe Type) -> [Stmt] -> Stmt
scalaCase v t = Fix . StCase v t

scalaTrait :: Name -> [Stmt] -> Stmt
scalaTrait n = Fix . StTrait n

scalaVal :: Name -> Bool -> Bool -> Maybe Type -> [Stmt] -> Stmt
scalaVal n o i t = Fix . StVal n o i t

scalaNew :: Name -> Bool -> [Stmt] -> [Stmt] -> Stmt
scalaNew n o as = Fix . StNew n o as

scalaField :: Stmt -> Name -> [Stmt] -> [Stmt] -> Stmt
scalaField n n' as = Fix . StField n n' as

scalaLambda :: [Stmt] -> [Stmt] -> Stmt
scalaLambda as = Fix . StLambda as

scalaPair :: Stmt -> Stmt -> Stmt
scalaPair v = Fix . StPair v

scalaArgument :: Name -> Maybe Type -> Maybe Stmt -> Stmt
scalaArgument n t = Fix . StArgument n t

scalaFor :: [Stmt] -> [Stmt] -> Stmt
scalaFor ss = Fix . StFor ss

scalaGenerator :: Name -> Stmt -> Stmt
scalaGenerator n = Fix . StGenerator n

scalaLiteral :: Show a => a -> Stmt
scalaLiteral = Fix . StLiteral . show

scalaIdent :: Name -> Stmt
scalaIdent = Fix . StIdent

scalaSome :: Stmt -> Stmt
scalaSome = Fix . StSome