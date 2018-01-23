{-# LANGUAGE TupleSections #-}

module Humbug.Nanopass.Headers
  ( cleanup
  , fiscionH
  , fiscionD
  ) where
--------------------------------------------------------------------------------
import Control.Applicative  ((<|>))
import Control.Arrow        ((***))
import Control.Monad        (liftM,(>=>))
import Data.Functor.Foldable
import Data.List            (findIndex, isPrefixOf, partition, uncons)
import Data.Maybe           (fromMaybe)
import Humbug.Thrift
import Humbug.Tokenize
import Humbug.Types
import System.FilePath.Posix(makeRelative, takeBaseName, (</>))
--------------------------------------------------------------------------------

cleanup :: Thrift -> Thrift
cleanup t = t { headers = filter (not . isCpp) $ headers t 
              , definitions = filter (not . isService) $ definitions t }

fiscionH :: Thrift -> ([Header], [Header])
fiscionH = partition isPackage . headers

fiscionD :: Thrift -> ([Definition], [Definition])
fiscionD = partition isConst . definitions

includes :: FilePath -> [Header] -> Eval [Thrift]
includes root = traverse $ tokenize . (root </>) . (\(Include f) -> f)

used :: [Definition] -> [Header] -> [Header]
used ds = filter isUsed
  where isUsed (Include f) = let n = takeBaseName f ++ "."
                             in cata (isUsed' n) ds
        isUsed' _ Nil = False
        isUsed' _ (Cons _ True) = True
        isUsed' n (Cons d _)
          | Const ft _ _ <- d = isPrefix n ft
          | Typedef ft _ <- d = isPrefix n ft
          | Struct _ fs <- d = any (isPrefix n . fieldType) fs
          | Union _ fs <- d = any (isPrefix n . fieldType) fs
          | Exception _ fs <- d = any (isPrefix n . fieldType) fs
          | otherwise = False

isPrefix :: String -> FieldType -> Bool
isPrefix n ft
  | FtMap kt vt <- ft = isPrefix n kt || isPrefix n vt
  | FtSet vt <- ft = isPrefix n vt
  | FtList vt <- ft = isPrefix n vt
  | FtNamed ident <- ft = isPrefixOf n ident
  | otherwise = False

allUniques :: Thrift -> Maybe (Identifier, Identifier)
allUniques (Thrift _ ds) = foldl u Nothing ds
  where u a b = a <|> uniquesD b

uniquesC :: [Definition] -> Maybe Identifier
uniquesC = uniques (==) . fmap (\(Const _ n _) -> n)

uniquesD :: Definition -> Maybe (Identifier, Identifier)
uniquesD d | (Enum n fs) <- d      = ((n,) . fst) <$> uniques (\a b -> snd a == snd b) fs
           | (Struct n fs) <- d    = ((n,) . fieldIdent) <$> uniques (==) fs
           | (Union n fs) <- d     = ((n,) . fieldIdent) <$> uniques (==) fs
           | (Exception n fs) <- d = ((n,) . fieldIdent) <$> uniques (==) fs
           | otherwise             = Nothing

uniques :: (a -> a -> Bool) -> [a] -> Maybe a
uniques eq = uncons >=> u
  where u ~(f, fs) = (findIndex (eq f) fs *> pure f) <|> uniques eq fs
