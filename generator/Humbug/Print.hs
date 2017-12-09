module Humbug.Print
( printScala
) where

import Data.Fix(cata)
import Data.List(concat, intersperse)
import Humbug.Scala

printScala :: Stmt -> String
printScala = cata print'

print' :: StmtF String -> String
print' (StPackage n is cs) = "package " ++ n ++ "\n\n" ++ join "\n\n" is ++ "\n\n" ++ join "\n\n" cs

print' (StImport n ns) = "import " ++ n ++ "." ++ showImports ns
  where
    showImports [] = "_"
    showImports (n : []) = concat $ showImport n []
    showImports ns = "{" ++ (concat $ intersperse "," $ foldr showImport [] ns) ++ "}"

    showImport (n, Nothing) res = n : res
    showImport (n, Just a) res = (n ++ "=>" ++ a) : res

print' (StPackageObject n stmts) = "package object " ++ n ++ showStatements stmts True

print' (StSealedTrait n p ccs) = "sealed trait " ++ n ++ showAncestor p ++ "\n" ++ join "\n" ccs

print' (StCaseClass n as ps) = "case class " ++ n ++ showArgs as ++ showAncestors ps

print' (StCaseObject n as p) = "case object " ++ n ++ showArgs as ++ showAncestor (Just p)

print' (StCompanionObject n p stmts) = "object " ++ n ++ showAncestor p ++ showStatements stmts True

print' (StMethod n override as rt stmts) = showOverride ++ "def " ++ n ++ showArgs as ++ showType rt ++ " = " ++ showStatements stmts False
  where
    showOverride = if override then "override " else ""
    
print' (StCase v vt stmts) = "case " ++ v ++ showType vt ++ " => " ++ showStatements stmts False

print' (StTrait n stmts) = "trait " ++ n ++ showStatements stmts False

print' (StVal n override implicit vt stmts) = showDecorators override implicit ++ "val " ++ showName n ++ showType vt ++ " = " ++ showStatements stmts False
  where
    showDecorators True False = "override "
    showDecorators False True = "implicit "
    showDecorators True True = "override implicit "
    showDecorators False False = ""

print' (StNew n caseclass vs stmts) = showNew ++ n ++ showArgs vs ++ showStatements stmts True
  where
    showNew = if caseclass then "" else "new "

print' (StField n n' as stmts) = showName n ++ "." ++ showName n' ++ showArgs as ++ showStatements stmts True

print' (StLambda ps stmts) = showArgs ps ++ " => " ++ showStatements stmts False

print' (StPair k v) = k ++ " -> " ++ v

print' (StArgument n t v) =
  let
    n' = showName n
    n'' = maybe n' (\t -> n' ++ ": " ++ t) t
  in
    maybe n'' (\v -> n'' ++ "= " ++ v) v

print' (StFor stmts ys) = "for " ++ showStatements stmts True ++ " yield " ++ showStatements ys False

print' (StGenerator n stmt) = showName n ++ " <- " ++ stmt

print' (StLiteral v) = v

print' (StIdent i) = showName i

print' (StSome x) = "Some(" ++ x ++ ")"

showStatements :: [String] -> Bool -> String
showStatements [] _ = []
showStatements [stmt] False = stmt
showStatements stmts _ = "{\n" ++ join "\n" stmts ++ "\n}"

showArgs :: [String] -> String
showArgs [] = ""
showArgs as = ("(" ++) $ (++ ")") $ join ",\n" as

showType :: Maybe Type -> String
showType = maybe "" (": " ++)

showAncestors :: [Name] -> String
showAncestors [] = ""
showAncestors ps = " extends " ++ join " with " ps

showAncestor :: Maybe Name -> String
showAncestor = maybe "" (" extends " ++)

showValue :: Maybe Value -> String
showValue = maybe "" (" = " ++) 

showName :: String -> String
showName n | elem n reserved = "`" ++ n ++ "`"
           | otherwise = n
  where 
    reserved = [ "abstract" 
               , "do" 
               , "finally" 
               , "import" 
               , "object" 
               , "return" 
               , "trait" 
               , "var"
               , "case" 
               , "catch" 
               , "class" 
               , "else" 
               , "extends" 
               , "false" 
               , "for" 
               , "forSome" 
               , "if"
               , "lazy" 
               , "match" 
               , "new" 
               , "override" 
               , "package" 
               , "private" 
               , "sealed" 
               , "super" 
               , "this" 
               , "try" 
               , "true" 
               , "type" 
               , "while" 
               , "with" 
               , "yield"
               , "def"
               , "final" 
               , "implicit" 
               , "null" 
               , "protected" 
               , "throw"
               , "val"
               , "_" 
               , ":"
               , "="
               , "=>" 
               , "<-" 
               , "<:"
               , "<%"
               , ">:" 
               , "#" 
               , "@"
               ]

join :: String -> [String] -> String
join sep = concat . intersperse sep