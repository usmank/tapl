module Parser ( parse
              ) where

import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Text.Parsec hiding (parse)

import Lexer
import AST

parse :: String -> Either ParseError [Term]
parse = runParser program emptyNamingContext ""

program :: Parser [Term]
program = do
  whiteSpace
  ts <- term `endBy1` semicolon
  whiteSpace
  eof
  return ts

-- A term is a left-associative chain of applications. This allows for proper precedence where application has the
-- highest precedence and associates to the left.
term :: Parser Term
term = chainl1 nonAppTerm (return TmApp)

-- Parse all terms which are not applications.
nonAppTerm :: Parser Term
nonAppTerm = parens term
         <|> abstraction
         <|> variable

abstraction :: Parser Term
abstraction = do
  lambda
  var <- identifier
  modifyState (addVar var)
  dot
  t1 <- term
  modifyState tail
  return $ TmAbs var t1

variable :: Parser Term
variable = do
  var <- identifier
  ctx <- getState
  let i = fromMaybe (error $ "Variable '" ++ var ++ "' is unbound.") (getVarIndex var ctx)
  return $ TmVar i
