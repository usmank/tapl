module Main where

import System.Environment
import System.IO

import AST
import Eval
import Parser
import TypeChecker

main :: IO ()
main = do
  args        <- getArgs
  (f, input)  <- getInput args
  terms       <- parseInput f input
  let results = fmap (processTerm emptyContext) terms
  putStrLn $ showTerms results

processTerm :: Context -> Term -> (Term, Type)
processTerm ctx ts = (eval ts ctx, typeOf ts ctx)

getInput :: [String] -> IO (FilePath, String)
getInput []   = (,) <$> pure "stdin" <*> getContents
getInput [f]  = (,) <$> pure f <*> readFile f
getInput _    = error "Incorrect number of arguments"

parseInput :: FilePath -> String -> IO [Term]
parseInput f s =
  case parse f s of
    Left e    -> print e >> error "Parse error"
    Right ts  -> return ts
