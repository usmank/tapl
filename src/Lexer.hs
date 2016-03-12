module Lexer ( arrow
             , colon
             , dot
             , equalSign
             , identifier
             , lambda
             , natural
             , parens
             , reserved
             , reservedOp
             , semicolon
             , whiteSpace
             ) where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

import AST

lexer = Token.makeTokenParser languageDef

languageDef = emptyDef { Token.reservedNames    = [ "if"
                                                  , "then"
                                                  , "else"
                                                  , "true"
                                                  , "false"
                                                  , "iszero"
                                                  , "Bool"
                                                  , "let"
                                                  , "in"
                                                  ]
                        , Token.reservedOpNames = [ "\\"
                                                  , "->"
                                                  , "="
                                                  ]
                        , Token.commentStart    = "{-"
                        , Token.commentEnd      = "-}"
                        , Token.commentLine     = "--"
                        , Token.identStart      = letter
                        , Token.identLetter     = alphaNum
                        , Token.caseSensitive   = True
                        }

arrow :: Parser ()
arrow = reservedOp "->"

colon :: Parser ()
colon = void $ Token.colon lexer

dot :: Parser String
dot = Token.dot lexer

equalSign :: Parser ()
equalSign = reservedOp "="

identifier :: Parser String
identifier = Token.identifier lexer

lambda :: Parser ()
lambda = reservedOp "\\"

natural :: Parser Integer
natural = Token.natural lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

semicolon :: Parser ()
semicolon = void $ Token.semi lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
