module Main where

import System.Environment
import System.IO
import Text.Parsec (parse)

import Core
import Parser
import Syntax

main :: IO ()
main = getArgs >>= getInput >>= parseInput >>= return . (fmap eval) >>= print

getInput :: [String] -> IO String
getInput []     = getContents
getInput [f]    = readFile f
getInput _      = error "Incorrect number of arguments"

parseInput :: String -> IO [Term]
parseInput s =
    case parse program "" s of
        Left e  -> print e >> error "Parse error"
        Right r -> return r
