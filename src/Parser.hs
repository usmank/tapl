module Parser ( parse
              ) where

import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Text.Parsec hiding (parse)

import Lexer
import AST

-- Parses the given string. First paramter is the source name and the second is the actual source.
parse :: String -> String -> Either ParseError [Term]
parse = runParser program emptyContext

-- Top level production, where a program consists of multiple terms, each terminated by a semicolon.
program :: Parser [Term]
program = do
  whiteSpace
  ts <- term `endBy1` semicolon
  whiteSpace
  eof
  return ts

-- A type annotation is a right-associative chain of types separated by arrows ('->').
typeAnnotation :: Parser Type
typeAnnotation = chainr1 atomicType $ arrow >> return TyArrow

atomicType :: Parser Type
atomicType = parens typeAnnotation
         <|> (reserved "Bool" >> return TyBool)

-- A term is a left-associative chain of applications. This allows for proper precedence where application has the
-- highest precedence and associates to the left.
term :: Parser Term
term = chainl1 nonAppTerm $ TmApp <$> getPosition

-- Parse all terms which are not applications.
nonAppTerm :: Parser Term
nonAppTerm = parens term
         <|> abstraction
         <|> variable
         <|> ifTerm
         <|> letBinding
         <|> atomicTerm

ifTerm :: Parser Term
ifTerm = do
  pos <- getPosition
  reserved "if"
  t1 <- term
  reserved "then"
  t2 <- term
  reserved "else"
  t3 <- term
  return $ TmIf pos t1 t2 t3

letBinding :: Parser Term
letBinding = do
  pos <- getPosition
  reserved "let"
  var <- identifier
  equalSign
  t1 <- term
  reserved "in"
  t2 <- term
  return $ TmLet pos var t1 t2

abstraction :: Parser Term
abstraction = do
  pos <- getPosition
  lambda
  var <- identifier
  colon
  typ <- typeAnnotation
  modifyState $ addBinding var (VarBind typ)
  dot
  t1 <- term
  modifyState tail
  return $ TmAbs pos var typ t1

variable :: Parser Term
variable = do
  pos <- getPosition
  var <- identifier
  ctx <- getState
  let i = fromMaybe (error $ "Variable '" ++ var ++ "' is unbound.") (getVarIndex var ctx)
  return $ TmVar pos i

atomicTerm :: Parser Term
atomicTerm = (reserved "true" >> TmTrue <$> getPosition)
         <|> (reserved "false" >> TmFalse <$> getPosition)
