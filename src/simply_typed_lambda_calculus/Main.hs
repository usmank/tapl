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
  args    <- getArgs
  input   <- getInput args
  terms   <- parseInput input
  let results = fmap (processTerm emptyContext) terms
  print $ fmap snd results
  putStrLn $ showTerms $ fmap fst results

processTerm :: Context -> Term -> (Term, Type)
processTerm ctx ts = (eval ts ctx, typeOf ts ctx)

getInput :: [String] -> IO String
getInput []   = getContents
getInput [f]  = readFile f
getInput _    = error "Incorrect number of arguments"

parseInput :: String -> IO [Term]
parseInput s =
  case parse s of
    Left e    -> print e >> error "Parse error"
    Right ts  -> return ts
