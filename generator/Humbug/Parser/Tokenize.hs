module Humbug.Tokenize
( tokenize
) where

import Humbug.Types
import Humbug.Utils.Strings
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.Parsec.Language(LanguageDef, javaStyle)

tokenize :: String -> Either ParseError Document
tokenize       = parse tokenize_impl "(unknown)"

tokenize_impl  = do { whiteSpace thrift
                    ; headers <- many header
                    ; definitions <- many definition
                    ; return $ Document headers definitions
                    }

thriftDef :: LanguageDef st
thriftDef       = javaStyle
                { nestedComments = False
                , identStart = letter <|> char '_'
                , identLetter = alphaNum <|> oneOf "_."
                -- there is no operator in the thrift language but this
                -- is mandatory
                , opStart = opLetter thriftDef
                , opLetter = oneOf "@"
                }

thrift :: TokenParser st
thrift         = makeTokenParser thriftDef

--- Headers
header         = include <|> namespace

include        = do { try (symbol thrift "include")
                    ; filename <- literal
                    ; return $ Include filename
                    }

cppInclude     = do { try (symbol thrift "cpp_include")
                    ; filename <- literal
                    ; return $ CppInclude filename
                    }

namespace      = do { try (symbol thrift "namespace")
                    ; scope <- scope thrift
                    ; ident <- identifier thrift
                    ; return $ Namespace scope ident
                    }

scope          = try (symbol thrift "*") >> return NsStar
               <|> try (symbol thrift "java") >> return NsJava
               -- the scala namespace is not standard and
               -- redundant with the java namespace but some
               -- thrift files use it
               <|> try (symbol thrift "scala") >> return NsJava
               <|> try (symbol thrift "rb") >> return NsRuby
               <|> try (symbol thrift "cpp") >> return NsCpp
               <|> try (symbol thrift "cocoa") >> return NsCocoa
               <|> try (symbol thrift "csharp") >> return NsCsharp
               <|> try (symbol thrift "py") >> return NsPython
               <|> try (symbol thrift "perl") >> return NsPerl

--- Definitions

definition     =   constant
               <|> typedef
               <|> enum
               <|> struct
               <|> union
               <|> exception
               <|> service

constant       = do { try (symbol thrift "const")
                    ; ft <- fieldType
                    ; ident <- identifier thrift
                    ; symbol thrift "="
                    ; v <- constValue
                    ; listSeparator
                    ; return $ Const ft ident v
                    }

typedef        = do { try (symbol thrift "typedef")
                    ; dt <- fieldType
                    ; ident <- identifier thrift
                    ; return $ Typedef dt ident
                    }

enum           = do { try (symbol thrift "enum")
                    ; ident <- identifier thrift
                    ; bindings <- braces thrift (enumBinding `sepEndBy` listSeparator)
                    ; return $ Enum ident bindings
                    }

enumBinding    = (,) <$> identifier thrift <*> optionMaybe (symbol thrift "=" *> intConstant)

struct         = do { try (symbol thrift "struct")
                    ; ident <- identifier thrift
                    ; fields <- braces thrift fields
                    ; return $ Struct ident fields
                    }

union          = do { try (symbol thrift "union")
                    ; ident <- identifier thrift
                    ; fields <- braces thrift fields
                    ; return $ Union ident fields
                    }

exception      = do { try (symbol thrift "exception")
                    ; ident <- identifier thrift
                    ; fields <- braces thrift fields
                    ; return $ Exception ident fields
                    }

service        = do { try (symbol thrift "service")
                    ; ident <- identifier thrift
                    ; super <- optionMaybe (do { symbol thrift "extends"
                                               ; ident <- identifier thrift
                                               ; return ident
                                               })
                    ; fns <- braces thrift (function `sepEndBy` listSeparator)
                    ; return $ Service ident super fns
                    }

--- Fields

fields         = (field `sepEndBy` listSeparator)

field          = Field <$> optionMaybe (string2int <$> many1 digit <* symbol thrift ":")
                       <*> optionMaybe (   do try (symbol thrift "required"); return Required
                                       <|> do try (symbol thrift "optional"); return Optional)
                       <*> fieldType
                       <*> identifier thrift
                       <*> optionMaybe (symbol thrift "=" *> constValue)

--- Functions

function       = Function <$> option False (do try (symbol thrift "oneway"); return True)
                          <*> functionType
                          <*> identifier thrift
                          <*> parens thrift fields
                          <*> optionMaybe (try (symbol thrift "throws") *> parens thrift fields)

functionType   = do { try (symbol thrift "void"); return LtVoid }
             <|> LtReturn <$> fieldType

--- Types

fieldType      = baseField <|> containerField <|> namedField

namedField     = FtNamed <$> identifier thrift

baseField      = FtBase <$> baseTypeIdentifier

baseTypeIdentifier = try (symbol thrift "bool") >> return BtBool
                    <|> try (symbol thrift "byte") >> return BtByte
                    <|> try (symbol thrift "i8") >> return BtInt8
                    <|> try (symbol thrift "i16") >> return BtInt16
                    <|> try (symbol thrift "i32") >> return BtInt32
                    <|> try (symbol thrift "i64") >> return BtInt64
                    <|> try (symbol thrift "double") >> return BtDouble
                    <|> try (symbol thrift "string") >> return BtString
                    <|> try (symbol thrift "binary") >> return BtBinary

containerField = FtContainer <$> (mapType <|> setType <|> listType)

mapType        = do { try (symbol thrift "map")
                    ; (kt, vt) <- angles thrift $ do { k <- fieldType
                                                     ; symbol thrift ","
                                                     ; v <- fieldType
                                                     ; return (k, v)
                                                     }
                    ; return $ CtMap kt vt
                    }

setType        = CtSet <$> (try (symbol thrift "set") *> angles thrift fieldType)

listType       = CtList <$> (try (symbol thrift "list") *> angles thrift fieldType)

--- Constant values
constValue     = doubleConstant <|> intConstant <|> stringConstant <|> namedConst <|> constList <|> constMap

intConstant    = CvInt <$> integer thrift

doubleConstant = CvDouble <$> float thrift

stringConstant = CvLiteral <$> literal

namedConst     = CvNamed <$> identifier thrift

constList      = CvList <$> brackets thrift (constValue `sepEndBy` listSeparator)

constMap       = CvMap <$> braces thrift (binding `sepEndBy` listSeparator)

binding        = (,) <$> constValue <*> (symbol thrift ":" *> constValue)

--- Basic definitions

literal       = stringLiteral thrift <|> sqLiteral

sqLiteral     = lexeme thrift (
                  do{ str <-  between (char '\'')
                                      (char '\'' <?> "end of string")
                                      (many stringChar)
                    ; return (foldr (:) "" str)
                    }
                  <?> "literal string")

stringChar    = noneOf "'"

listSeparator = optionMaybe (lexeme thrift $ (char ';' <|> char ','))
