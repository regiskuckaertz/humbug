{-# LANGUAGE UnicodeSyntax #-}

module Humbug.Types
( Eval
, runEval
) where

import Control.Monad.State.Strict(StateT, runStateT)
import Control.Monad.Trans.Except
import Humbug.Scala(Name)
import Text.ParserCombinators.Parsec.Error
import qualified Data.Map as Map

type State = Map.Map FilePath (Name, [Name])

type Eval a = ExceptT ParseError (StateT State IO) a

runEval ∷ Eval a → IO (Either ParseError a, State)
runEval ev = runStateT (runExceptT ev) Map.empty