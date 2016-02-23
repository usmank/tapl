module Lexer ( dot
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
                                                  , "flase"
                                                  , "iszero"
                                                  ]
                        , Token.reservedOpNames = [ "succ"
                                                  , "pred"
                                                  , "iszero"
                                                  , "\\"
                                                  ]
                        , Token.commentStart    = "{-"
                        , Token.commentEnd      = "-}"
                        , Token.commentLine     = "--"
                        , Token.identStart      = letter
                        , Token.identLetter     = alphaNum
                        , Token.caseSensitive   = True
                        }

dot :: Parser ()
dot = void $ Token.dot lexer 

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

semicolon :: Parser String
semicolon = Token.semi lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
