module Humbug.Compile
( compile
) where

import Data.Char(toUpper)
import Data.List(elemIndices, intersperse)
import qualified Data.Map.Strict as Map
import Humbug.Scala
import Humbug.Thrift

compile :: Document -> Map.Map FilePath [Stmt]
compile (Document hs ds) = let 
  pkg = buildPackage hs
  imps = buildImports hs
  defs = map compile' ds
  in foldr Map.union Map.empty defs

compile' :: Definition -> Map.Map FilePath [Stmt]
compile' (Const ft ident cv) = Map.empty
compile' (Typedef ft ident) = Map.singleton ident (buildTypedef ident ft)
compile' (Enum ident vs) = Map.singleton ident (buildEnum ident vs)
compile' (Struct ident fs) = Map.singleton ident (buildStruct ident fs)
compile' (Union ident fs) = Map.empty
compile' (Exception ident fs) = Map.empty
compile' (Service ident pident fns) = Map.empty

buildPackage :: [Header] -> Maybe Stmt
buildPackage [] = Nothing
buildPackage (Namespace NsJava ident : _) = Just $ scalaPackage ident
buildPackage (Namespace NsStar ident : _) = Just $ scalaPackage ident
buildPackage (_ : hs) = buildPackage hs

buildImports :: [Header] -> [Stmt]
buildImports [] = []
buildImports (Include lit : hs) = scalaImport lit [] : buildImports hs
buildImports (_ : hs) = buildImports hs

buildTypedef :: Identifier -> FieldType -> [Stmt]
buildTypedef ident ft = let
  ft' = buildType ft
  cv = scalaCaseClass ident [("value", Just ft', Nothing)] ["AnyVal", "TTypeDef"]
  ex = "TTypeDefCodec[" ++ ident ++ "," ++ (buildType ft) ++ "]"
  menc = scalaMethod "encode" True [] Nothing [scalaField (scalaIdent "_") "value" [] []]
  mdec = scalaMethod "decode" True [] Nothing [scalaNew ident False [scalaIdent "_"] []]
  o = scalaCompanionObject ident (Just ex) [menc, mdec]
  in [cv, o]

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
      zfids = zip fids $ vs
      zfs = zip fs $ vs
      wits = map buildWitness zfids
      imps = map buildFieldCodec zfids
      maps = foldr buildDefaultValue [] zfs
      --- TODO
      hmap = scalaNew "HMap[TFieldCodec]" True maps []
      defs = scalaVal "defaults" True False Nothing [hmap]
      --- TODO
      hmap' = scalaNew "HMap[TFieldCodec]" True [] []
      lenc = scalaLambda [("x", Nothing, Nothing)] [hmap']
      --- TODO
      ldec = scalaLambda [("m", Nothing, Nothing)] []
      menc = scalaMethod "encode" True [] Nothing [lenc]
      mdec = scalaMethod "decode" True [] Nothing [ldec]
      p = "TStructCodec[" ++ ident ++ "]"
      in scalaCompanionObject ident (Just p) (wits ++ imps ++ [defs, menc, mdec])
    buildDefaultValue (Field _ (Just Optional) ft _ cv, wid) vs = let
      mv = maybe "None" (\v -> "Some(" ++ (buildValue v) ++ ")") cv
      v = scalaPair ("w" ++ (show wid) ++ ".value") mv Nothing
      in (v : vs)
    buildDefaultValue (Field _ _ ft _ (Just cv), wid) vs = let
      v = scalaPair ("w" ++ (show wid) ++ ".value") (buildValue cv) Nothing
      in (v : vs)
    buildDefaultValue _ vs = vs

buildUnion :: Identifier -> [Field] -> Map.Map FilePath [Stmt]
buildUnion ident fs = let
  st = scalaSealedTrait ident (Just "TStruct") []
  ccs = buildCaseClasses
  vs = iterate (+1) 1
  fids = fst $ foldr buildFieldIds ([], -1) fs
  zfs = zip fids fs
  zfids = zip fids $ vs
  wits = map buildWitness zfids
  imps = map buildFieldCodec zfids
  menc = scalaMethod "encode" True [] Nothing $ map buildEncode zfs
  ldec = scalaLambda [("m", Nothing, Nothing)] $ map buildDecode zfs
  --- TODO
  mdec = scalaMethod "decode" True [] Nothing []
  co = scalaCompanionObject ident (Just $ "TStructCodec[" ++ ident ++ "]") (wits ++ imps ++ [menc, mdec])
  in Map.singleton ident ((st : ccs) ++ [co])
  where
    buildCaseClasses = map (\f ->
      case f of 
        (Field fid _ ft fn _) -> scalaCaseClass (buildClassName fn) [(fn, Just $ buildType ft, Nothing)] [ident]
      ) fs
    buildClassName (c : cs) = (toUpper c : cs)
    buildEncode (fid, Field _ _ _ fn _) = scalaCase ((buildClassName fn) ++ "(x)") Nothing [scalaNew "HMap[TFieldCodec]" True [] []]
    --- TODO
    buildDecode (fid, (Field _ _ _ ident _)) = let
      f = scalaField (scalaIdent "m") "get" [scalaLiteral fid] []
      in scalaField f "map" [scalaNew ident True [scalaIdent "_"] []] []

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
  is -> drop (last is) ident

buildValue :: ConstValue -> String
buildValue (CvInt i) = show i
buildValue (CvDouble d) = show d
buildValue (CvLiteral lit) = case lit of
  '\'' : rs -> '"' : (init rs) ++ ['"']
  _ -> lit
buildValue (CvNamed ident) = ident
buildValue (CvList cs) = let
  cs' = map buildValue cs
  in "List(" ++ (concat $ intersperse "," cs') ++ ")"
buildValue (CvMap cs) = let
  cs' = unzip cs
  cks = map buildValue $ fst cs'
  cvs = map buildValue $ snd cs'
  cs'' = map (\(k,v) -> k ++ "->" ++ v) $ zip cks cvs
  in "List(" ++ (concat $ intersperse "," cs'') ++ ")"

buildField :: Field -> Argument
buildField (Field _ fr ft ident fv) = let
  ft' = buildType ft
  in case fr of
    (Just Optional) -> let 
      fv' = maybe "None" (\fv -> "Some(" ++ (buildValue fv) ++ ")") fv
                     in (ident, Just ("Option[" ++ buildType ft ++ "]"), Just fv')
    _ -> let
      fv' = maybe Nothing (\fv -> Just $ buildValue fv) fv
      in (ident, Just (buildType ft), fv')

buildFieldIds :: Field -> ([Int], Int) -> ([Int], Int)
buildFieldIds (Field (Just fid) _ _ _ _) (fids, fid') = (fid : fids, fid')
buildFieldIds _ (fids, fid) = (fid : fids, fid - 1)

buildWitness :: (Int, Int) -> Stmt
buildWitness (fid, var) = let
  wit = scalaNew "Witness" True [scalaLiteral fid] []
  in scalaVal ("w" ++ show var) False False Nothing [wit]

buildFieldCodec :: (Int, Int) -> Stmt
buildFieldCodec (fid, var) = let
  fn = "TFieldCodec[w" ++ (show var) ++ "," ++ "Nothing" ++ "]"
  fc = scalaNew fn False [] []
  in scalaVal ("r" ++ show var) False True Nothing [fc]

buildException :: Identifier -> [Field] -> Map.Map FilePath [Stmt]
buildException ident fs = Map.empty

buildConst :: FieldType -> Identifier -> ConstValue -> Map.Map FilePath [Stmt]
buildConst ft ident cv = let
  cnst = scalaVal ident False False (Just $ buildType ft) []
  pkg = scalaPackageObject "tmp" [cnst]
  in Map.singleton "package" [pkg]