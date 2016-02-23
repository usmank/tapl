module Main where

import System.Environment
import System.IO

import Eval
import Parser
import AST

main :: IO ()
main = getArgs >>= getInput >>= parseInput >>= return . fmap eval >>= putStrLn . showTerms

getInput :: [String] -> IO String
getInput []   = getContents
getInput [f]  = readFile f
getInput _    = error "Incorrect number of arguments"

parseInput :: String -> IO [Term]
parseInput s =
  case parse s of
    Left e  -> print e >> error "Parse error"
    Right ts -> return ts
