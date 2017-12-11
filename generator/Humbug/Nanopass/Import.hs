{-# LANGUAGE UnicodeSyntax #-}

module Humbug.Nanopass.Import
( process 
) where

import Data.Fix
import Data.List
import qualified Data.Map as Map

import Humbug.Scala

process ∷ Name → Stmt → [Name]
process prefix = cata (process' prefix)

process' ∷ Name → StmtF [Name] → [Name]
process' prefix (StVal _ _ _ (Just t) _) =
  let
    lprefix = length prefix
  in 
    if (take lprefix t) == prefix
      then [drop (lprefix + 1) t]
      else []

process' prefix (StPackageObject _ ns) = concat ns
process' prefix (StCaseClass _ ns _) = concat ns
process' prefix (StCaseObject _ ns _) = concat ns
process' prefix (StSealedTrait _ _ ns) = concat ns
process' prefix (StCompanionObject _ _ ns) = concat ns
process' prefix (StMethod _ _ ns _ _) = concat ns
process' prefix (StTrait _ ns) = concat ns
process' _ _ = []