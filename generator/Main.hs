module Main where

import Control.Monad((>=>))
import Control.Monad.IO.Class
import Humbug.Compile
import Humbug.Print
import Humbug.Scala
import Humbug.Thrift
import Humbug.Tokenize
import Humbug.Types
import Data.List(uncons, intersperse, isSuffixOf)
import System.Directory(getCurrentDirectory, listDirectory)
import qualified Data.Map.Strict as Map

loadFiles :: IO [String]
loadFiles = do
  d <- getCurrentDirectory
  fs <- listDirectory d
  let ts = filter (isSuffixOf ".thrift") fs 
  traverse readFile ts

saveFiles :: Map.Map FilePath [Stmt] -> Eval (Map.Map FilePath ())
saveFiles m = 
  let
    save = \f stmts -> liftIO $ writeFile (f ++ ".scala") $ concat $ intersperse "\n" $ fmap printScala stmts
  in Map.traverseWithKey save m

--- Compiles ./*.thrift -> ./*.scala
main = do
  ts <- loadFiles
  cs <- traverse (runEval . (tokenize >=> compile >=> saveFiles)) ts
  return ()