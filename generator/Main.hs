module Main where

import Control.Monad((>=>), guard)
import Control.Monad.IO.Class
import Humbug.Compile
import Humbug.Print
import Humbug.Scala
import Humbug.Thrift
import Humbug.Tokenize
import Humbug.Types
import Data.List(uncons, intersperse, isSuffixOf)
import System.Directory
  ( getCurrentDirectory
  , createDirectoryIfMissing
  , withCurrentDirectory
  , listDirectory
  , makeAbsolute
  )
import System.Environment(getArgs)
import qualified Data.Map.Strict as Map

loadFiles :: IO [String]
loadFiles = do
  d <- getCurrentDirectory
  fs <- listDirectory d
  let ts = filter (isSuffixOf ".thrift") fs 
  traverse readFile ts

saveFiles :: FilePath -> Map.Map FilePath [Stmt] -> Eval (Map.Map FilePath ())
saveFiles dst m = 
  let
    save = \f stmts -> liftIO $ withCurrentDirectory dst $ writeFile (f ++ ".scala") $ concat $ intersperse "\n\n" $ fmap printScala stmts
  in do
    Map.traverseWithKey save m

run :: FilePath -> FilePath -> Eval ()
run src dst = 
  do
    asrc <- liftIO $ makeAbsolute src
    adst <- liftIO $ makeAbsolute dst
    _ <- liftIO $ createDirectoryIfMissing True adst
    ts <- liftIO $ withCurrentDirectory asrc loadFiles
    traverse (tokenize >=> compile >=> saveFiles adst) ts
    return ()

--- Compiles ./*.thrift -> ./*.scala
main = do
  (src : dst : []) <- getArgs
  runEval $ run src dst
