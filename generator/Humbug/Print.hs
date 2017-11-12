module Humbug.Print
( printScala
) where

import Data.Fix
import Data.List(concat, intersperse)
import Humbug.Scala

printScala :: Stmt -> String
printScala = concat . intersperse "\n" . cata print'

print' :: StmtF [String] -> [String]
print' (StPackage n) = ["package " ++ n]

print' (StImport n ns) = ["import " ++ n ++ "." ++ showImports ns]
  where
    showImports [] = "_"
    showImports (n : []) = concat $ showImport n []
    showImports ns = "{" ++ (concat $ intersperse "," $ foldr showImport [] ns) ++ "}"

    showImport (n, Nothing) res = n : res
    showImport (n, Just a) res = (n ++ "=>" ++ a) : res

print' (StPackageObject n stmts) = ("package object " ++ n) : (showStatements " " $ concat stmts)

print' (StSealedTrait n p ccs) = ("sealed trait " ++ n ++ showAncestor p) : (concat ccs)

print' (StCaseClass n as p) = ["case class " ++ n ++ (showArgs as) ++ (showAncestor p)]

print' (StCaseObject n as p) = ["case object " ++ n ++ (showArgs as) ++ (showAncestor (Just p))]

print' (StCompanionObject n p stmts) = ("object " ++ n ++ (showAncestor p)) : (showStatements " " $ concat stmts)

print' (StMethod n override as rt stmts) = (showOverride ++ "def " ++ n ++ (showArgs as) ++ (showType rt)) : (showStatements " = " $ concat stmts)
  where
    showOverride = if override then "override " else ""
    
print' (StCase v vt stmts) = ("case " ++ v ++ (showType vt)) : (showStatements " => " $ concat stmts)

print' (StTrait n stmts) = ("trait " ++ n) : (showStatements " " $ concat stmts)

print' (StVal n override implicit stmts) = ((showDecorators override implicit) ++ "val " ++ n) :  (showStatements " = " $ concat stmts)
  where
    showDecorators True False = "override "
    showDecorators False True = "implicit "
    showDecorators True True = "override implicit "
    showDecorators False False = ""

print' (StNew n caseclass vs stmts) = ((showNew caseclass) ++ n ++ (showArgs vs)) : (showStatements " " $ concat stmts)
  where
    showNew True = ""
    showNew False = "new "

print' (StLambda ps stmts) = (showArgs ps) : (showStatements " => " $ concat stmts)

print' (StPair k v) = [k ++ " -> " ++ (showValue v)]
  where
    showValue (v, t) = maybe v ((v ++ ": ") ++) t

print' (StForC stmts y) = (showStatements "for " $ concat stmts) ++ [" yield " ++ y]

print' (StForV n v) = [n ++ " <- " ++ v]

print' (StLiteral v) = [v]

showStatements :: String -> [String] -> [String]
showStatements _ [] = []
showStatements pfx (stmt : []) = [pfx ++ stmt]
showStatements pfx stmts = pfx : "{" : stmts ++ ["}"]

showArgs :: [Argument] -> String
showArgs [] = ""
showArgs as = ("(" ++) $ (++ ")") $ concat $ intersperse "," $ map showArg as

showArg :: Argument -> String
showArg (n, Nothing, Nothing) = n
showArg (n, Just t, Nothing) = n ++ " : " ++ t
showArg (n, Nothing, Just v) = n ++ " = " ++ v
showArg (n, Just t, Just v) = n ++ " : " ++ t ++ " = " ++ v

showType :: Maybe Type -> String
showType = maybe "" (": " ++)

showAncestor :: Maybe Name -> String
showAncestor = maybe "" (" extends " ++)

showValue :: Maybe Value -> String
showValue = maybe "" (" = " ++) 