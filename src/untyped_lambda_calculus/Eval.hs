module Eval ( eval
            ) where

import AST

eval :: Term -> Term
eval t =
  let t' = eval' t
  in  if t == t' then t else eval t'

eval' :: Term -> Term
-- E-AppAbs
eval' (TmApp (TmAbs _ t12) v2)
  | isVal v2  = shift (-1) $ substitute 0 (shift 1 v2) t12
eval' (TmApp t1 t2)
  -- E-App2
  | isVal t1  = TmApp t1 (eval' t2)
  -- E-App1
  | otherwise = TmApp (eval' t1) t2
eval' t = t

isVal :: Term -> Bool
isVal (TmAbs _ _) = True
isVal _           = False

shift :: Integer -> Term -> Term
shift = shift' 0

shift' :: Integer -> Integer -> Term -> Term
shift' c d v@(TmVar i)
  | i >= c    = TmVar $ i + d
  | otherwise = v
shift' c d (TmAbs n t1)   = TmAbs n $ shift' (c + 1) d t1
shift' c d (TmApp t1 t2)  = TmApp (shift' c d t1) (shift' c d t2)

substitute :: VarIndex -> Term -> Term -> Term
substitute j s k@(TmVar i)
  | i == j    = s
  | otherwise = k
substitute j s (TmAbs n t1)   = TmAbs n $ substitute (j + 1) (shift 1 s) t1
substitute j s (TmApp t1 t2)  = TmApp (substitute j s t1) (substitute j s t2)
