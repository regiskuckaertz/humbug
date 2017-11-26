module Humbug.Types
( Eval
, runEval
) where

import Control.Monad.Trans.Except
import Text.ParserCombinators.Parsec.Error

type Eval a = ExceptT ParseError IO a

runEval :: Eval a -> IO (Either ParseError a)
runEval = runExceptT
