module Lexer where

import Text.Parsec.Language
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token

lexer = Token.makeTokenParser languageDef

languageDef = emptyDef { Token.reservedNames    = [ "if"
                                                  , "then"
                                                  , "else"
                                                  , "true"
                                                  , "flase"
                                                  , "iszero"
                                                  ]
                        , Token.reservedOpNames = [ "succ"
                                                  , "pred"
                                                  , "iszero"
                                                  ]
                        }

natural :: Parser Integer
natural = Token.natural lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

semi :: Parser String
semi = Token.semi lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
