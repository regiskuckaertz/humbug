module Main where

import Control.Monad((>=>), guard)
import Control.Monad.IO.Class
import Control.Monad.Except
import Humbug.Compile
import Humbug.Print
import Humbug.Scala
import Humbug.Thrift
import Humbug.Tokenize
import Humbug.Types
import Data.Foldable(foldrM)
import Data.List(uncons, intersperse, isSuffixOf)
import System.Directory
  ( getCurrentDirectory
  , createDirectoryIfMissing
  , withCurrentDirectory
  , listDirectory
  , canonicalizePath
  )
import System.Environment(getArgs)
import System.FilePath.Posix(makeRelative, takeDirectory, (</>))
import qualified Data.Map.Strict as Map

isInclude :: Header -> Bool
isInclude (Include _) = True
isInclude _ = False

getIncludes :: Header -> IO FilePath
getIncludes (Include f) = canonicalizePath f

loadThrift :: FilePath -> FilePath -> Map.Map FilePath Document -> Eval (Map.Map FilePath Document)
loadThrift r f m = 
  if (Map.member f m)
    then return m
    else do
      t@(Document hs ds) <- ExceptT $ withCurrentDirectory r $ tokenize f
      let r' = takeDirectory f
      let m' = Map.singleton f t `Map.union` m
      is <- liftIO $ traverse (withCurrentDirectory r' . getIncludes) $ filter isInclude hs
      foldrM (loadThrift $ r </> r') m' is

saveFiles :: FilePath -> Map.Map FilePath [Stmt] -> Eval (Map.Map FilePath ())
saveFiles dst m = Map.traverseWithKey save m
  where
    save f stmts = do
      _ <- liftIO $ createDirectoryIfMissing True dst 
      liftIO $ withCurrentDirectory dst $ writeFile (f ++ ".scala") $ concat $ intersperse "\n\n" $ fmap printScala stmts

run :: FilePath -> FilePath -> Eval ()
run f d = 
  do
    asrc <- liftIO $ canonicalizePath f
    adst <- liftIO $ canonicalizePath d
    _ <- liftIO $ createDirectoryIfMissing True adst
    ts <- loadThrift (takeDirectory asrc) asrc Map.empty
    ss <- Map.traverseWithKey (\k v -> compile v >>= saveFiles (adst </> (makeRelative (takeDirectory asrc) (takeDirectory k)))) ts
    return ()

--- Takes two args:
--- - the Thrift file to compile
--- - the destination folder for compiled assets
main = do
  (f : d : []) <- getArgs
  runEval $ run f d
