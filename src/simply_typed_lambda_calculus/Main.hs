module Main where

import System.Environment
import System.IO

import AST
import Eval
import Parser
import TypeChecker

-- TODO:
--  [ ] Include final type for each term.
main :: IO ()
main = do
  args        <- getArgs
  (f, input)  <- getInput args
  terms       <- parseInput f input
  let results = fmap (processTerm emptyContext) terms
  print $ fmap snd results
  putStrLn $ showTerms $ fmap fst results

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
