{-# LANGUAGE UnicodeSyntax #-}

module Humbug.Tokenize
( tokenize
) where

import Control.Monad.IO.Class
import Control.Monad.Except
import Humbug.Types
import Humbug.Utils.Strings
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.Parsec.Language(LanguageDef, javaStyle)
import qualified Humbug.Thrift as T

tokenize ∷ FilePath → Eval T.Thrift
tokenize t     = do
  _ ← liftIO $ putStrLn ("Parsing " ++ t)
  ExceptT $ liftIO $ parseFromFile tokenize_impl t

tokenize_impl  = do { whiteSpace thrift
                    ; headers ← many header
                    ; definitions ← many definition
                    ; return $ T.Thrift headers definitions
                    }

thriftDef ∷ LanguageDef st
thriftDef       = javaStyle
                { nestedComments = False
                , identStart = letter <|> char '_'
                , identLetter = alphaNum <|> oneOf "_."
                -- there is no operator in the thrift language but this
                -- is mandatory
                , opStart = opLetter thriftDef
                , opLetter = oneOf "@"
                }

thrift ∷ TokenParser st
thrift         = makeTokenParser thriftDef

--- Headers
header         = include <|> namespace

include        = do { try (symbol thrift "include")
                    ; filename ← literal
                    ; return $ T.Include filename
                    }

cppInclude     = do { try (symbol thrift "cpp_include")
                    ; filename ← literal
                    ; return $ T.CppInclude filename
                    }

namespace      = do { try (symbol thrift "namespace")
                    ; scp ← scope
                    ; ident ← identifier thrift
                    ; return $ T.Namespace scp ident
                    }

scope          = (try (symbol thrift "*") >> return T.NsStar)
               <|> (try (symbol thrift "java") >> return T.NsJava)
               <|> (try (symbol thrift "rb") >> return T.NsRuby)
               <|> (try (symbol thrift "cpp") >> return T.NsCpp)
               <|> (try (symbol thrift "cocoa") >> return T.NsCocoa)
               <|> (try (symbol thrift "csharp") >> return T.NsCsharp)
               <|> (try (symbol thrift "py") >> return T.NsPython)
               <|> (try (symbol thrift "perl") >> return T.NsPerl)
               <?> "unrecognised namespace scope"

--- Definitions

definition     =   constant
               <|> typedef
               <|> enum
               <|> struct
               <|> union
               <|> exception
               <|> service
               <?> "unregonised definition"

constant       = do { try (symbol thrift "const")
                    ; ft ← fieldType
                    ; ident ← identifier thrift
                    ; symbol thrift "="
                    ; v ← constValue
                    ; listSeparator
                    ; return $ T.Const ft ident v
                    }

typedef        = do { try (symbol thrift "typedef")
                    ; dt ← fieldType
                    ; ident ← identifier thrift
                    ; return $ T.Typedef dt ident
                    }

enum           = do { try (symbol thrift "enum")
                    ; ident ← identifier thrift
                    ; bindings ← braces thrift (enumBinding `sepEndBy` listSeparator)
                    ; return $ T.Enum ident bindings
                    }

enumBinding    = (,) <$> identifier thrift <*> optionMaybe (symbol thrift "=" *> intConstant)

struct         = do { try (symbol thrift "struct")
                    ; ident ← identifier thrift
                    ; fields ← braces thrift fields
                    ; return $ T.Struct ident fields
                    }

union          = do { try (symbol thrift "union")
                    ; ident ← identifier thrift
                    ; fields ← braces thrift fields
                    ; return $ T.Union ident fields
                    }

exception      = do { try (symbol thrift "exception")
                    ; ident ← identifier thrift
                    ; fields ← braces thrift fields
                    ; return $ T.Exception ident fields
                    }

service        = do { try (symbol thrift "service")
                    ; ident ← identifier thrift
                    ; super ← optionMaybe (do { symbol thrift "extends"
                                               ; ident ← identifier thrift
                                               ; return ident
                                               })
                    ; fns ← braces thrift (function `sepEndBy` listSeparator)
                    ; return $ T.Service ident super fns
                    }

--- Fields

fields         = (field `sepEndBy` listSeparator)

field          = T.Field <$> optionMaybe (string2int <$> many1 digit <* symbol thrift ":")
                         <*> optionMaybe (   do try (symbol thrift "required"); return T.Required
                                         <|> do try (symbol thrift "optional"); return T.Optional)
                         <*> fieldType
                         <*> identifier thrift
                         <*> optionMaybe (symbol thrift "=" *> constValue)

--- Functions

function       = T.Function <$> option False (do try (symbol thrift "oneway"); return True)
                            <*> functionType
                            <*> identifier thrift
                            <*> parens thrift fields
                            <*> optionMaybe (try (symbol thrift "throws") *> parens thrift fields)

functionType   = do { try (symbol thrift "void"); return T.LtVoid }
             <|> T.LtReturn <$> fieldType
             <?> "Unrecorgnised function return type"

--- Types

fieldType      = baseField <|> containerField <|> namedField

namedField     = T.FtNamed <$> identifier thrift

baseField      = (try (symbol thrift "bool") >> return T.FtBool)
                  <|> (try (symbol thrift "byte") >> return T.FtByte)
                  <|> (try (symbol thrift "i8") >> return T.FtInt8)
                  <|> (try (symbol thrift "i16") >> return T.FtInt16)
                  <|> (try (symbol thrift "i32") >> return T.FtInt32)
                  <|> (try (symbol thrift "i64") >> return T.FtInt64)
                  <|> (try (symbol thrift "double") >> return T.FtDouble)
                  <|> (try (symbol thrift "string") >> return T.FtString)
                  <|> (try (symbol thrift "binary") >> return T.FtBinary)

containerField = mapType <|> setType <|> listType

mapType        = do { try (symbol thrift "map")
                    ; (kt, vt) ← angles thrift $ do { k ← fieldType
                                                     ; symbol thrift ","
                                                     ; v ← fieldType
                                                     ; return (k, v)
                                                     }
                    ; return $ T.FtMap kt vt
                    }

setType        = T.FtSet <$> (try (symbol thrift "set") *> angles thrift fieldType)

listType       = T.FtList <$> (try (symbol thrift "list") *> angles thrift fieldType)

--- Constant values
constValue     = doubleConstant <|> intConstant <|> stringConstant <|> namedConst <|> constList <|> constMap

intConstant    = T.CvInt <$> fromIntegral <$> integer thrift

doubleConstant = T.CvDouble <$> float thrift

stringConstant = T.CvLiteral <$> literal

namedConst     = T.CvNamed <$> identifier thrift

constList      = T.CvList <$> brackets thrift (constValue `sepEndBy` listSeparator)

constMap       = T.CvMap <$> braces thrift (binding `sepEndBy` listSeparator)

binding        = (,) <$> constValue <*> (symbol thrift ":" *> constValue)

--- Basic definitions

literal       = stringLiteral thrift <|> sqLiteral

sqLiteral     = lexeme thrift (
                  do{ str ←  between (char '\'')
                                      (char '\'' <?> "end of string")
                                      (many stringChar)
                    ; return (foldr (:) "" str)
                    }
                  <?> "literal string")

stringChar    = noneOf "'"

listSeparator = optionMaybe (lexeme thrift $ (char ';' <|> char ','))
