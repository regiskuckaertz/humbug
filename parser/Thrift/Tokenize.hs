module Thrift.Tokenize
( tokenize
) where

import Thrift.Types
import Utils.Strings
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

namespace      = phpNamespace <|> xsdNamespace <|> simpleNamespace

phpNamespace   = do { try (symbol thrift "php_namespace")
                    ; filename <- literal
                    ; return $ PhpNamespace filename }

xsdNamespace   = do { try (symbol thrift "xsd_namespace")
                    ; filename <- literal
                    ; return $ XsdNamespace filename }

simpleNamespace= do { try (symbol thrift "namespace")
                    ; scope <-  try (symbol thrift "*")
                            <|> try (symbol thrift "java")
                            -- the scala namespace is not standard and
                            -- redundant with the java namespace but some
                            -- thrift files use it
                            <|> try (symbol thrift "scala")
                            <|> try (symbol thrift "rb")
                            <|> try (symbol thrift "cpp")
                            <|> try (symbol thrift "cocoa")
                            <|> try (symbol thrift "csharp")
                            <|> try (symbol thrift "py")
                            <|> try (symbol thrift "perl")
                            <|> try (symbol thrift "smalltalk.category")
                            <|> try (symbol thrift "smalltalk.prefix")
                    ; ident <- if scope == "smalltalk.category" then stIdentifier
                                                                else identifier thrift
                    ; return $ Namespace scope ident
                    }

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
                    ; dt <- definitionType
                    ; ident <- identifier thrift
                    ; return $ Typedef dt ident
                    }

definitionType = (Base <$> baseTypeIdentifier) <|> (Container <$> (mapType <|> setType <|> listType))

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

field          = Field <$> optionMaybe (FieldID . string2int <$> many1 digit <* symbol thrift ":")
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

functionType   = do { try (symbol thrift "void"); return VoidType }
             <|> ReturnType <$> fieldType

--- Types

fieldType      = baseField <|> containerField <|> namedField

namedField     = NamedField <$> identifier thrift

baseField      = BaseField <$> baseTypeIdentifier

baseTypeIdentifier = try (symbol thrift "bool")
                    <|> try (symbol thrift "byte")
                    <|> try (symbol thrift "i8")
                    <|> try (symbol thrift "i16")
                    <|> try (symbol thrift "i32")
                    <|> try (symbol thrift "i64")
                    <|> try (symbol thrift "double")
                    <|> try (symbol thrift "string")
                    <|> try (symbol thrift "binary")
                    <|> try (symbol thrift "slist")

containerField = ContainerField <$> (mapType <|> setType <|> listType)

mapType        = do { try (symbol thrift "map")
                    ; (kt, vt) <- angles thrift $ do { k <- fieldType
                                                     ; symbol thrift ","
                                                     ; v <- fieldType
                                                     ; return (k, v)
                                                     }
                    ; return $ MapType (kt, vt)
                    }

setType        = SetType <$> (try (symbol thrift "set") *> angles thrift fieldType)

listType       = ListType <$> (try (symbol thrift "list") *> angles thrift fieldType)

--- Constant values
constValue     = doubleConstant <|> intConstant <|> stringConstant <|> namedConst <|> constList <|> constMap

intConstant    = IntConstant <$> integer thrift

doubleConstant = DoubleConstant <$> float thrift

stringConstant = LiteralString <$> literal

namedConst     = NamedConst <$> identifier thrift

constList      = ConstList <$> brackets thrift (constValue `sepEndBy` listSeparator)

constMap       = ConstMap <$> braces thrift (binding `sepEndBy` listSeparator)

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

stIdentifier  = lexeme thrift (
                  do{ start <- letter <|> char '_'
                    ; next  <- many (alphaNum <|> oneOf ".-_")
                    ; return start : next
                    }
                  <?> "smalltalk identifier")
