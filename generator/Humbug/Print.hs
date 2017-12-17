{-# LANGUAGE UnicodeSyntax #-}

module Humbug.Print
( printScala
) where

import Data.Fix(cata)
import Humbug.Scala
import Prelude hiding(print)
import Text.PrettyPrint

printScala ∷ Stmt → String
printScala = render . cata print

print (StPackage n is cs) = text "package" <+> text n $$ vcat is $$ vcat cs

print (StImport n ns) = text "import" <+> text n <> dot <> imports ns
  where imports [] = char '_'
        imports ((n, Nothing) : []) = text n
        imports ns = braces $ nest 1 $ vcat $ punctuate comma $ fmap import1 ns

        import1 (n, Nothing) = text n
        import1 (n, Just a) = text n <+> darr <+> text a

print (StPackageObject n stmts) = text "package object" <+> text n <+> statements stmts

print (StSealedTrait n p ccs) = text "sealed trait" <+> text n <+> ancestor p <+> vcat ccs

print (StCaseClass n as ps) = text "case class" <+> text n <> args as <+> ancestors ps

print (StCaseObject n as p) = text "case object" <+> text n <> args as <+> anc
  where anc = ancestor $ Just p

print (StCompanionObject n p stmts) = text "object" <+> text n <+> ancestor p <+> statements stmts

print (StMethod n o as rt stmts) = override o <+> text "def" <+> text n <> args as <> showType rt <+> equals <+> statements stmts
  where override True = text "override"
        override False = empty
    
print (StCase v vt stmts) = text "case" <+> text v <> showType vt <+> darr <+> statements stmts

print (StTrait n stmts) = text "trait" <+> text n <+> statements stmts

print (StVal n override implicit vt stmts) = decorators override implicit <+> text "val" <+> name n <+> showType vt <+> equals <+> statements stmts
  where decorators True False = text "override"
        decorators False True = text "implicit"
        decorators True True = text "override" <+> text "implicit"
        decorators False False = empty

print (StNew n cc vs stmts) = new cc <+> text n <> args vs <+> statements stmts
  where new True = empty 
        new False = text "new"

print (StField n n' as stmts) = n <> dot <> name n' <> args as <+> statements stmts

print (StLambda ps stmts) = args ps <+> darr <+> statements stmts

print (StPair k v) = k <+> text "->" <+> v

print (StArgument n t v) = name n <+> showType t <+> value v

print (StFor stmts ys) = text "for" <+> statements stmts <+> text "yield" <+> statements ys

print (StGenerator n stmt) = name n <+> text "<-" <+> stmt

print (StLiteral v) = text v

print (StIdent i) = name i

print (StSome x) = text "Some" <> parens x

statements ∷ [Doc] → Doc
statements = braces . nest 1 . vcat

args ∷ [Doc] → Doc
args = parens . nest 1 . vcat . punctuate comma

showType ∷ Maybe Type → Doc
showType = maybe empty (\t -> colon <+> text t)

ancestors ∷ [Name] → Doc
ancestors [] = empty
ancestors ps = text "extends" <+> (vcat . punctuate with . fmap text) ps
  where with = text "with"

ancestor ∷ Maybe Name → Doc
ancestor = maybe empty (\a -> text "extends" <+> text a)

value ∷ Maybe Doc → Doc
value = maybe empty (\v -> equals <+> v)

dot :: Doc
dot = char '.'

darr :: Doc
darr = text "=>"

name ∷ String → Doc
name n | elem n reserved = char '`' <> text n <> char '`'
       | otherwise = text n
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
