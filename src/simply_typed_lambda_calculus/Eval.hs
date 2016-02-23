module Eval ( eval
            ) where

import AST

-- Repeatedly applies eval to a term until no evaluation rules apply.
eval :: Term -> Context -> Term
eval t ctx =
  let t' = eval' t
  in  if t == t' then t else eval t' ctx

-- TODO:
--  [ ] Refactor to take a Context.
eval' :: Term -> Term
-- E-AppAbs
eval' (TmApp (TmAbs _ _ t12) v2)
  | isVal v2  = shift (-1) $ substitute 0 (shift 1 v2) t12
eval' (TmApp t1 t2)
  -- E-App2
  | isVal t1  = TmApp t1 (eval' t2)
  -- E-App1
  | otherwise = TmApp (eval' t1) t2
-- E-IfTrue
eval' (TmIf TmTrue t2 _) = t2
-- E-IfFalse
eval' (TmIf TmFalse _ t3) = t3
-- E-If
eval' (TmIf t1 t2 t3) = TmIf (eval' t1) t2 t3
-- Return the input term itself if no evaluation rules apply.
eval' t = t

-- Returns true if the given term is a value.
isVal :: Term -> Bool
isVal (TmAbs _ _ _) = True
isVal (TmTrue)      = True
isVal (TmFalse)     = True
isVal _             = False

-- Shifts all de Bruijn indicies in the given term by the given amount.
shift :: Integer -> Term -> Term
shift = shift' 0

-- Shifts all de Bruijn indicies above a cutoff in the given term by the given amount.
shift' :: Integer -> Integer -> Term -> Term
shift' c d v@(TmVar i)
  | i >= c    = TmVar $ i + d
  | otherwise = v
shift' c d (TmAbs n typ t1) = TmAbs n typ $ shift' (c + 1) d t1
shift' c d (TmApp t1 t2)    = TmApp (shift' c d t1) (shift' c d t2)
shift' c d (TmIf t1 t2 t3)  = TmIf (shift' c d t1) (shift' c d t2) (shift' c d t3)
shift' _ _ t                = t

-- Performs a capture avoid substitution of s for j in the given term.
substitute :: VarIndex -> Term -> Term -> Term
substitute j s k@(TmVar i)
  | i == j    = s
  | otherwise = k
substitute j s (TmAbs n typ t1) = TmAbs n typ $ substitute (j + 1) (shift 1 s) t1
substitute j s (TmApp t1 t2)    = TmApp (substitute j s t1) (substitute j s t2)
substitute j s (TmIf t1 t2 t3)  = TmIf (substitute j s t1) (substitute j s t2) (substitute j s t3)
substitute _ _ t                = t
