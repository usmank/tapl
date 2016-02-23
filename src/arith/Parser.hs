module Parser where

import Control.Applicative ((*>))
import Text.Parsec
import Text.Parsec.String (Parser)

import Lexer
import Syntax

program :: Parser [Term]
program = whiteSpace *> term `endBy1` semi

term :: Parser Term
term = try ifTerm
   <|> try appTerm

ifTerm :: Parser Term
ifTerm = do
    reserved "if"
    t1 <- term
    reserved "then"
    t2 <- term
    reserved "else"
    t3 <- term
    return $ TmIf t1 t2 t3

appTerm :: Parser Term
appTerm = atomicTerm
      <|> do { reservedOp "succ"; t <- atomicTerm; return $ TmSucc t; }
      <|> do { reservedOp "pred"; t <- atomicTerm; return $ TmPred t; }
      <|> do { reservedOp "iszero"; t <- atomicTerm; return $ TmIsZero t; }

atomicTerm :: Parser Term
atomicTerm = parens term
         <|> (reserved "true" >> return TmTrue)
         <|> (reserved "false" >> return TmFalse)
         <|> (natural >>= return . naturalToSucc)

naturalToSucc :: Integer -> Term
naturalToSucc 0 = TmZero
naturalToSucc n = TmSucc $ naturalToSucc $ n - 1
