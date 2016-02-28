module Util ( err
            ) where

import Text.Parsec

import AST

err :: SourcePos -> String -> a
err sp s =
  let sn = sourceName sp
      sl = show $ sourceLine sp
      sc = show $ sourceColumn sp
  in  error $ sn ++ ":" ++ sl ++ ":" ++ sc ++ "\n\t" ++ s
