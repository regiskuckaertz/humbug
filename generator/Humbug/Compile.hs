{-# LANGUAGE UnicodeSyntax #-}

module Humbug.Compile
( compile
) where

import Control.Arrow((&&&))
import Data.Functor.Foldable(Fix(..), para, unfix)
import Data.List(partition, unzip)
import Data.Tuple(swap)
import Humbug.Scala
import Humbug.Thrift
import Humbug.Types

compile ∷ Thrift → [Stmt]
compile = para compile'

compile' ∷ ThriftF (Thrift, [Stmt]) → [Stmt]
compile' (ThDocument is ds) = docs
  where docs = fmap (buildDocument is) ds'
        ds' = uncurry (++) . partition isConst $ ds
        isConst d | Fix(ThConst _ _ _) <- fst d = True
                  | otherwise                   = False

compile' (ThInclude lit) = [scalaImportPlaceholder lit]

compile' (ThNamespace NsJava ident) = [scalaPackage ident]
compile' (ThNamespace NsStar ident) = [scalaPackage ident]
compile' (ThNamespace _ _) = [scalaNoop]

compile' (ThConst ct cn cv) = [scalaVal cn False False ft fv]
  where ft = Just $ buildType ct
        fv = [buildValue cv]

compile' (ThTypedef ft ident) = [vc, co]
  where ft' = buildType ft
        v = scalaArgument "value" (Just ft') Nothing
        vc = scalaCaseClass ident [v] ["AnyVal", "TTypeDef"]
        co = scalaCompanionObject ident (Just e) [menc, mdec]
        e = "TTypeDefCodec[" ++ ident ++ "," ++ ft' ++ "]"
        menc = scalaMethod "encode" True [] Nothing [scalaField u "value" [] []]
        mdec = scalaMethod "decode" True [] Nothing [scalaNew ident False [u] []]
        u = scalaIdent "_"

compile' (ThEnum ident fs) = [st, co]
  where st = scalaSealedTrait ident (Just "TEnum") cos
        cos = fmap ((\x → scalaCaseObject x [] ident) . fst) fs
        co = scalaCompanionObject ident cop [menc, mdec]
        cop = Just $ "TEnumCodec[" ++ ident ++ "]"
        menc = scalaMethod "encode" True [] Nothing encs
        mdec = scalaMethod "decode" True [] Nothing decs
        encs = fst $ foldr buildEncode ([], 0) fs
        decs = fst $ foldr buildDecode ([], 0) fs
        buildEncode (ident, Just (CvInt v)) (stmts, _) = ((scalaCase ident Nothing [scalaLiteral v]) : stmts, v + 1)
        buildEncode (ident, _)              (stmts, v) = ((scalaCase ident Nothing [scalaLiteral v]) : stmts, v + 1)
        buildDecode (ident, Just (CvInt v)) (stmts, _) = ((scalaCase (show v) Nothing [scalaIdent ident]) : stmts, v + 1)
        buildDecode (ident, _)              (stmts, v) = ((scalaCase (show v) Nothing [scalaIdent ident]) : stmts, v + 1)

compile' (ThStruct ident fs) = [cc, co]
  where cc = scalaCaseClass ident (foldMap snd fs) ["TStruct"]
        co = scalaCompanionObject ident (Just $ "TStructCodec[" ++ ident ++ "]") (ws ++ is ++ [defs, menc, mdec])
        (ws, is) = unzip $ fmap (buildWitness &&& buildCodec) fws
        defs = scalaVal "defaults" True False Nothing [hds]
        hds = scalaNew "HMap[TFieldCodec]" True ds []
        ds = foldMap buildDefaultValue fws
        menc = scalaMethod "encode" True [] Nothing [lenc]
        mdec = scalaMethod "decode" True [] Nothing [ldec]
        lenc = scalaLambda [scalaArgument "x" Nothing Nothing] [henc]
        ldec = scalaLambda [scalaArgument "m" Nothing Nothing] [fdec]
        henc = scalaNew "HMap[TFieldCodec]" True ps []
        fdec = scalaFor ps' [y]
        (ps, ps') = unzip $ fmap (buildPair &&& buildFor) fws
        y = scalaNew ident True (fmap (scalaLiteral . getIdent) fws) []
        fws = zip freshIds . fmap (unfix . fst) $ fs

compile' (ThUnion ident fs) = [st, co]
  where st = scalaSealedTrait ident (Just "TUnion") ccs
        ccs = fmap cc fws
        cc (tf, f) = scalaCaseClass (getIdent tf) f [ident]
        co = scalaCompanionObject ident (Just $ "TUnionCodec[" ++ ident ++ "]") (ws ++ is ++ [menc, mdec])
        (ws, is) = unzip $ fmap ((buildWitness &&& buildCodec) . fst) fws
        menc = scalaMethod "encode" True [] Nothing enc
        mdec = scalaMethod "decode" True [] Nothing [scalaNoop]
        (enc, dec) = unzip $ fmap ((buildCase &&& buildGet) . fst) fws
        fws = uncurry zip . swap . fmap (zip freshIds . fmap unfix) . swap . unzip $ fs
        
compile' (ThException ident fs) = [cc, co]
  where n = ""
        is = []
        cc = scalaCaseClass ident (foldMap snd fs) ["TException"]
        co = scalaNoop

compile' (ThField _ fr ft ident fv) | Just Optional ← fr = [scalaArgument ident t1 v1]
                                    | otherwise           = [scalaArgument ident t2 v2]
  where t1 = Just $ "Option[" ++ buildType ft ++ "]"
        v1 = Just $ maybe (scalaIdent "None") (scalaSome . buildValue) fv
        t2 = Just $ buildType ft
        v2 = maybe Nothing (Just . buildValue) fv

compile' _ = [scalaNoop]

buildDocument ∷ [(Thrift, [Stmt])] → (Thrift, [Stmt]) → Stmt
buildDocument is cu = scalaDocument p $ is' ++ snd cu
  where (p, is') = foldr buildPackageAndImports (scalaPackage "default", []) is 

buildPackageAndImports ∷ (Thrift, [Stmt]) → (Stmt, [Stmt]) → (Stmt, [Stmt])
buildPackageAndImports i r@(p, is)
  | Fix(ThNamespace NsJava _) ← fst i = (head $ snd i, is)
  | Fix(ThNamespace NsStar _) ← fst i
  , Fix(StPackage "default")  ← p     = (head $ snd i, is)
  | Fix(ThInclude _)          ← fst i = (p, snd i ++ is)
  | otherwise                          = r

buildType ∷ FieldType → String
buildType FtBool = "Boolean"
buildType FtByte = "Byte"
buildType FtInt8 = "Byte"
buildType FtInt16 = "Short"
buildType FtInt32 = "Int"
buildType FtInt64 = "Long"
buildType FtDouble = "Double"
buildType FtString = "String"
buildType FtBinary = "ByteVector"
buildType (FtMap ftk ftv) = "Map[" ++ buildType ftk ++ "," ++ buildType ftv ++ "]"
buildType (FtSet ft) = "Set[" ++ buildType ft ++ "]"
buildType (FtList ft) = "List[" ++ buildType ft ++ "]"
buildType (FtNamed ident) = ident

buildValue ∷ ConstValue → Stmt
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
  cs'' = map (\(k,v) → scalaPair k v) $ zip cks cvs
  in scalaNew "Map" True cs'' []

freshIds ∷ [Int]
freshIds = iterate (+1) 1

buildWitness ∷ (Int, ThriftF Thrift) → Stmt
buildWitness (wid, ThField (Just fid) _ _ _ _) =
  let w = scalaNew "Witness" True [scalaLiteral fid] []
  in scalaVal ("w" ++ show wid) False False Nothing [w]

buildCodec ∷ (Int, ThriftF Thrift) → Stmt
buildCodec (wid, ThField _ fr ft _ _) = scalaVal ("r" ++ show wid) False True Nothing [fc]
  where fc = scalaNew fn False [] []
        fn = "TFieldCodec[w" ++ show wid ++ ".T," ++ ft'' ++ "]"
        ft' = buildType ft
        ft'' | Just Optional ← fr = "Option[" ++ ft' ++ "]"
             | otherwise           = ft''

buildDefaultValue ∷ (Int, ThriftF Thrift) → [Stmt]
buildDefaultValue (wid, ThField _ fr _ _ cv) | Just Optional ← fr = [scalaPair k v]
                                             | Just cv'      ← cv = [scalaPair k $ buildValue cv']
                                             | otherwise          = []
  where k = scalaField i "value" [] []
        i = scalaIdent $ "w" ++ show wid
        v = maybe (scalaIdent "None") (\v → scalaNew "Some" True [buildValue v] []) cv
        
buildPair ∷ (Int, ThriftF Thrift) → Stmt
buildPair (wid, ThField _ _ _ ident _) = scalaPair k v
  where k = witnessValue wid
        v = scalaField (scalaIdent "x") ident [] []

buildFor ∷ (Int, ThriftF Thrift) → Stmt
buildFor (wid, ThField _ _ _ ident _) = scalaGenerator ident r
  where r = scalaField r' "orElse" [r''] []
        r' = scalaField (scalaIdent "m") "get" [w] []
        r'' = scalaField (scalaIdent "defaults") "get" [w] []
        w = witnessValue wid

buildCase ∷ (Int, ThriftF Thrift) → Stmt
buildCase (wid, ThField _ _ _ ident _) = scalaCase (ident ++ "(x)") Nothing [n]
  where n = scalaNew "HMap[TFieldCodec]" True [scalaPair (witnessValue wid) $ scalaIdent "x"] []

buildGet ∷ (Int, ThriftF Thrift) → Stmt
buildGet (wid, ThField _ _ _ ident _) = scalaField f "map" [n] []
  where f = scalaField (scalaIdent "m") "get" [witnessValue wid] []
        n = scalaNew ident True [scalaIdent "_"] []

getIdent ∷ (Int, ThriftF Thrift) → String
getIdent (_, ThField _ _ _ ident _) = ident

witnessValue ∷ Int → Stmt
witnessValue i = scalaField (scalaIdent $ "w" ++ show i) "value" [] []
