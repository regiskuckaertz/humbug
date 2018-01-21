{-# LANGUAGE UnicodeSyntax #-}

module Main where
--------------------------------------------------------------------------------
import           Control.Monad             ((>=>), guard)
import           Control.Monad.IO.Class
import           Control.Monad.Except
import           Control.Monad.State.Strict(StateT)
import qualified Data.Map.Strict as Map
import           Data.Foldable             (foldrM)
import           Data.List                 (uncons, intersperse, isSuffixOf)
import           Humbug.Compile
import           Humbug.Print
import           Humbug.Scala
import           Humbug.Thrift
import           Humbug.Tokenize
import           Humbug.Types
import           System.Directory
  ( getCurrentDirectory
  , setCurrentDirectory
  , createDirectoryIfMissing
  , withCurrentDirectory
  , listDirectory
  , canonicalizePath
  )
import           System.Environment       (getArgs)
import           System.FilePath.Posix    (makeRelative, takeDirectory, (</>))
--------------------------------------------------------------------------------

isInclude ∷ Header → Bool
isInclude (Include _) = True
isInclude _ = False

getIncludes ∷ Header → IO FilePath
getIncludes (Include f) = canonicalizePath f

load ∷ FilePath → FilePath → Map.Map FilePath Document → Eval (Map.Map FilePath Document)
load r f m = 
  if (Map.member f m)
    then return m
    else do
      d ← liftIO $ getCurrentDirectory
      liftIO $ setCurrentDirectory r
      t@(Document hs ds) ← tokenize f
      liftIO $ setCurrentDirectory d
      let r' = takeDirectory f
      let m' = Map.singleton f t `Map.union` m
      is ← liftIO $ traverse (withCurrentDirectory r' . getIncludes) $ filter isInclude hs
      foldrM (load $ r </> r') m' is

saveFiles ∷ FilePath → Map.Map FilePath Stmt → Eval (Map.Map FilePath ())
saveFiles dst m = Map.traverseWithKey save m
  where
    save f stmt = do
      _ ← liftIO $ createDirectoryIfMissing True dst 
      liftIO $ withCurrentDirectory dst $ writeFile (f ++ ".scala") $ printScala stmt

run ∷ FilePath → FilePath → Eval ()
run f d = 
  do
    asrc ← liftIO $ canonicalizePath f
    adst ← liftIO $ canonicalizePath d
    liftIO $ createDirectoryIfMissing True adst
    ts ← load (takeDirectory asrc) asrc Map.empty
    ss ← Map.traverseWithKey (compileAndSave asrc adst) ts
    return ()
  where
    compileAndSave asrc adst src thrift = 
      compile thrift >>= (saveFiles $ adst </> makeRelative (takeDirectory asrc) (takeDirectory src))

--- Takes two args:
--- - the Thrift file to compile
--- - the destination folder for compiled assets
main = do
  (f : d : []) ← getArgs
  runEval $ run f d
