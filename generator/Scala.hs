module Humbug.Scala(
  Stmt
) where

import Data.Fix

type Name = String

type Type = String

type Value = String

data StmtF a b c = StPackage Name
               | StImport Name [(Name, Maybe Name)]
               | StPackageObject Name [a]
               | StSealedTrait Name [Name] [a]
               | StCaseClass Name [a] (Maybe Name)
               | StCaseObject Name [a] Name
               | StCompanionObject Name Name [a]
               | StMethod Name Boolean [(Name, Type, Maybe Value)] (Maybe Type) [a]
               | StCase Value (Maybe Type) [a]
               | StTrait Name [a]
               | StArgument Name Type (Maybe Value)
               | StVal Name Boolean Boolean [a]
               | StNew Name Boolean [Value] [a]
               | StLambda [Name] [a]

type Stmt a = Fix (StmtF a)