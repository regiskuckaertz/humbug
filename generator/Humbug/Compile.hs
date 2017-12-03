module Humbug.Compile
( compile
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Char(toUpper)
import Data.List(elemIndices, foldr1, intersperse)
import qualified Data.Map.Strict as Map
import Humbug.Scala
import Humbug.Thrift
import Humbug.Types

compile :: Document -> Eval (Map.Map FilePath [Stmt])
compile (Document hs ds) = let 
  pkg = buildPackage hs
  imps = foldMap buildImports hs
  defs = foldMap compile' ds
  cnts = foldMap (compile'' pkg) ds
  pobj = buildPackageObject pkg cnts
  in
    do  _ <- liftIO $ putStrLn ("Compiling Scala...")
        ExceptT $ return $ Right $ mappend (prelude pkg <$> defs) (prelude' pkg <$> pobj)
  where
    prelude pkg stmts = scalaPackage pkg : stmts
    prelude' pkg stmts = scalaPackage pkg' : stmts
      where
        pkg' = case (elemIndices '.' pkg) of
          [] -> pkg
          is -> take (last is) pkg

compile' :: Definition -> Map.Map FilePath [Stmt]
compile' (Typedef ft ident) = Map.singleton ident (buildTypedef ident ft)
compile' (Enum ident vs) = Map.singleton ident (buildEnum ident vs)
compile' (Struct ident fs) = Map.singleton ident (buildStruct ident fs)
compile' (Union ident fs) = Map.singleton ident (buildUnion ident fs)
compile' (Exception ident fs) = mempty
compile' (Service ident pident fns) = mempty
compile' _ = mempty

compile'' :: String -> Definition -> [Stmt]
compile'' pkg (Const ct cn cv) = 
  let
    ft = Just $ buildType ct
    fv = buildValue cv
  in [scalaVal cn False False ft [fv]]
compile'' pkg _  = []

buildPackage :: [Header] -> String
buildPackage [] = "humbug.sample"
buildPackage (Namespace NsJava ident : _) = ident
buildPackage (Namespace NsStar ident : _) = ident
buildPackage (_ : hs) = buildPackage hs

buildPackageObject :: String -> [Stmt] -> Map.Map FilePath [Stmt]
buildPackageObject pkg cnts = 
  case cnts of
    [] -> mempty
    _ -> Map.singleton "package" [(scalaPackageObject (buildPackageName pkg) cnts)]

buildImports :: Header -> [Stmt]
buildImports (Include lit) = [scalaImportPlaceholder lit]
buildImports _ = []

buildTypedef :: Identifier -> FieldType -> [Stmt]
buildTypedef ident ft = let
  ft' = buildType ft
  vc = scalaCaseClass ident [scalaArgument "value" (Just ft') Nothing] ["AnyVal", "TTypeDef"]
  ex = "TTypeDefCodec[" ++ ident ++ "," ++ (buildType ft) ++ "]"
  u = scalaIdent "_"
  menc = scalaMethod "encode" True [] Nothing [scalaField u "value" [] []]
  mdec = scalaMethod "decode" True [] Nothing [scalaNew ident False [u] []]
  o = scalaCompanionObject ident (Just ex) [menc, mdec]
  in [vc, o]

buildEnum :: Identifier -> [(Identifier, Maybe ConstValue)] -> [Stmt]
buildEnum ident fs = [buildSealedTrait, buildCompanionObject]
  where
    buildSealedTrait = let
      cos = map (\x -> scalaCaseObject x [] ident) $ map fst fs
      in scalaSealedTrait ident (Just "TEnum") cos
    buildCompanionObject = let
      encs = fst $ foldr buildEncode ([], 0) fs
      decs = fst $ foldr buildDecode ([], 0) fs
      menc = scalaMethod "encode" True [] Nothing encs
      mdec = scalaMethod "decode" True [] Nothing decs
      p = "TEnumCodec[" ++ ident ++ "]"
      in scalaCompanionObject ident (Just p) [menc, mdec]
    buildEncode (ident, Just (CvInt v)) (stmts, _) = ((scalaCase ident Nothing [scalaLiteral v]) : stmts, v + 1)
    buildEncode (ident, _) (stmts, v) = ((scalaCase ident Nothing [scalaLiteral v]) : stmts, v + 1)
    buildDecode (ident, Just (CvInt v)) (stmts, _) = ((scalaCase (show v) Nothing [scalaIdent ident]) : stmts, v + 1)
    buildDecode (ident, _) (stmts, v) = ((scalaCase (show v) Nothing [scalaIdent ident]) : stmts, v + 1)

buildStruct :: Identifier -> [Field] -> [Stmt]
buildStruct ident fs = let
  cc = scalaCaseClass ident (map buildField fs) ["TStruct"]
  co = buildCompanionObject
  in [cc, co]
  where
    buildCompanionObject = let
      vs = iterate (+1) 1
      fids = fst $ foldr buildFieldIds ([], -1) fs
      zfids = zip fids vs
      zfs = zip fs vs
      wits = map buildWitness zfids
      imps = map buildFieldCodec zfs
      maps = foldMap buildDefaultValue zfs
      hmap = scalaNew "HMap[TFieldCodec]" True maps []
      defs = scalaVal "defaults" True False Nothing [hmap]
      maps' = map buildWitnessField zfs
      hmap' = scalaNew "HMap[TFieldCodec]" True maps' []
      lenc = scalaLambda [scalaArgument "x" Nothing Nothing] [hmap']
      as = map buildAssignment zfs
      fns = map (\(Field _ _ _ ident _) -> ident) fs
      c = scalaNew ident True (map scalaIdent fns) []
      for = scalaFor as [c]
      ldec = scalaLambda [scalaArgument "m" Nothing Nothing] [for]
      menc = scalaMethod "encode" True [] Nothing [lenc]
      mdec = scalaMethod "decode" True [] Nothing [ldec]
      p = "TStructCodec[" ++ ident ++ "]"
      in scalaCompanionObject ident (Just p) (wits ++ imps ++ [defs, menc, mdec])
    buildDefaultValue (Field _ (Just Optional) ft _ cv, wid) = 
      let
        mv = maybe (scalaIdent "None") (\v -> scalaNew "Some" True [buildValue v] []) cv
        v = scalaPair 
          (scalaField (scalaIdent ("w" ++ show wid)) "value" [] [])
          mv
      in [v]
    buildDefaultValue (Field _ _ ft _ (Just cv), wid) = 
      let
        v = scalaPair 
          (scalaField (scalaIdent ("w" ++ show wid)) "value" [] []) 
          (buildValue cv)
      in [v]
    buildDefaultValue _ = []
    buildWitnessField (Field _ _ _ ident _, wid) =
      scalaPair (scalaField (scalaIdent $ "w" ++ show wid) "value" [] []) (scalaField (scalaIdent "x") ident [] [])
    buildAssignment (Field _ _ _ ident _, wid) = let
      w = scalaField (scalaIdent ("w" ++ show wid)) "value" [] []
      f = scalaField (scalaIdent "m") "get" [w] []
      f' = scalaField (scalaIdent "defaults") "get" [w] []
      f'' = scalaField f "orElse" [f'] []
      in scalaGenerator ident f''

buildUnion :: Identifier -> [Field] -> [Stmt]
buildUnion ident fs = let
  st = scalaSealedTrait ident (Just "TUnion") []
  ccs = map buildCaseClass fs
  vs = iterate (+1) 1
  fids = fst $ foldr buildFieldIds ([], -1) fs
  zfids = zip fids vs
  zfs = zip fs vs
  wits = map buildWitness zfids
  imps = map buildFieldCodec zfs
  menc = scalaMethod "encode" True [] Nothing $ map buildEncode zfs
  ldec = scalaLambda [scalaArgument "m" Nothing Nothing] $ [foldr1 (\fa fb -> scalaField fa "orElse" [fb] []) $ map buildDecode zfs]
  mdec = scalaMethod "decode" True [] Nothing [ldec]
  co = scalaCompanionObject ident (Just $ "TUnionCodec[" ++ ident ++ "]") (wits ++ imps ++ [menc, mdec])
  in (st : ccs) ++ [co]
  where
    buildCaseClass (Field fid _ ft fn _) = 
      scalaCaseClass (buildClassName fn) [scalaArgument fn (Just $ buildType ft) Nothing] [ident]
    buildClassName (c : cs) = (toUpper c : cs)
    buildEncode (Field _ _ _ fn _, wid) = let
      n = scalaNew "HMap[TFieldCodec]" True [scalaPair (scalaField (scalaIdent $ "w" ++ show wid) "value" [] []) (scalaIdent "x")] []
      in scalaCase ((buildClassName fn) ++ "(x)") Nothing [n]
    buildDecode (Field _ _ _ ident _, wid) = let
      f = scalaField (scalaIdent "m") "get" [scalaField (scalaIdent $ "w" ++ show wid) "value" [] []] []
      n = scalaNew (buildClassName ident) True [scalaIdent "_"] []
      in scalaField f "map" [n] []

buildType :: FieldType -> String
buildType (FtBase BtBool) = "Boolean"
buildType (FtBase BtByte) = "Byte"
buildType (FtBase BtInt8) = "Byte"
buildType (FtBase BtInt16) = "Short"
buildType (FtBase BtInt32) = "Int"
buildType (FtBase BtInt64) = "Long"
buildType (FtBase BtDouble) = "Double"
buildType (FtBase BtString) = "String"
buildType (FtBase BtBinary) = "ByteVector"
buildType (FtContainer (CtMap ftk ftv)) = "Map[" ++ (buildType ftk) ++ "," ++ (buildType ftv) ++ "]"
buildType (FtContainer (CtSet ft)) = "Set[" ++ (buildType ft) ++ "]"
buildType (FtContainer (CtList ft)) = "List[" ++ (buildType ft) ++ "]"
buildType (FtNamed ident) = case (elemIndices '.' ident) of
  [] -> ident
  is -> drop (last is + 1) ident

buildValue :: ConstValue -> Stmt
buildValue (CvInt i) = scalaLiteral i
buildValue (CvDouble d) = scalaLiteral d
buildValue (CvLiteral lit) = scalaLiteral $ drop 1 $ init lit
buildValue (CvNamed ident) = scalaIdent ident
buildValue (CvList cs) = let
  cs' = map buildValue cs
  in scalaNew "List" True cs' []
buildValue (CvMap cs) = let
  cs' = unzip cs
  cks = map buildValue $ fst cs'
  cvs = map buildValue $ snd cs'
  cs'' = map (\(k,v) -> scalaPair k v) $ zip cks cvs
  in scalaNew "Map" True cs'' []

buildField :: Field -> Stmt
buildField (Field _ fr ft ident fv) = let
  ft' = buildType ft
  in case fr of
    (Just Optional) -> let 
      fv' = maybe (scalaIdent "None") (\fv -> scalaSome $ buildValue fv) fv
      in scalaArgument ident (Just ("Option[" ++ buildType ft ++ "]")) (Just fv')
    _ -> let
      fv' = maybe Nothing (\fv -> Just $ buildValue fv) fv
      in scalaArgument ident (Just $ buildType ft) fv'

buildFieldIds :: Field -> ([Int], Int) -> ([Int], Int)
buildFieldIds (Field (Just fid) _ _ _ _) (fids, fid') = (fid : fids, fid')
buildFieldIds _ (fids, fid) = (fid : fids, fid - 1)

buildWitness :: (Int, Int) -> Stmt
buildWitness (fid, var) = let
  wit = scalaNew "Witness" True [scalaLiteral fid] []
  in scalaVal ("w" ++ show var) False False Nothing [wit]

buildFieldCodec :: (Field, Int) -> Stmt
buildFieldCodec (Field _ fr ft _ _, var) = let
  ft' = buildType ft
  ft'' = case fr of
    Just Optional -> "Option[" ++ ft' ++ "]"
    _ -> ft'
  fn = "TFieldCodec[w" ++ (show var) ++ ".T," ++ ft'' ++ "]"
  fc = scalaNew fn False [] []
  in scalaVal ("r" ++ show var) False True Nothing [fc]

buildException :: Identifier -> [Field] -> Map.Map FilePath [Stmt]
buildException ident fs = Map.empty

buildConst :: Definition -> [Stmt] -> [Stmt]
buildConst (Const ft ident cv) ss = (scalaVal ident False False (Just $ buildType ft) []) : ss
buildConst _ ss = ss

buildPackageName :: String -> String
buildPackageName pkg = case (elemIndices '.' pkg) of
  [] -> pkg
  is -> drop ((last is) + 1) pkg
