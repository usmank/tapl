module Eval ( eval
            ) where

import AST

-- Repeatedly applies eval to a term until no evaluation rules apply.
eval :: Term -> Context -> Term
eval t ctx =
  let t' = eval' t
  in  if t == t' then t else eval t' ctx

eval' :: Term -> Term
eval' (TmApp _ (TmAbs _ _ _ t12) v2)
  | isVal v2  = shift (-1) $ substitute 0 (shift 1 v2) t12
eval' (TmApp sp t1 t2)
  | isVal t1  = TmApp sp t1 (eval' t2)
  | otherwise = TmApp sp (eval' t1) t2
eval' (TmIf _ (TmTrue _) t2 _) = t2
eval' (TmIf _ (TmFalse _)_ t3) = t3
eval' (TmIf sp t1 t2 t3) = TmIf sp (eval' t1) t2 t3
eval' t = t

-- Returns true if the given term is a value.
isVal :: Term -> Bool
isVal (TmAbs _ _ _ _) = True
isVal (TmTrue _)      = True
isVal (TmFalse _)     = True
isVal _               = False

-- Shifts all de Bruijn indicies in the given term by the given amount.
shift :: Integer -> Term -> Term
shift = shift' 0

-- Shifts all de Bruijn indicies above a cutoff in the given term by the given amount.
shift' :: Integer -> Integer -> Term -> Term
shift' c d v@(TmVar sp i)
  | i >= c    = TmVar sp $ i + d
  | otherwise = v
shift' c d (TmAbs sp n typ t1)  = TmAbs sp n typ $ shift' (c + 1) d t1
shift' c d (TmApp sp t1 t2)     = TmApp sp (shift' c d t1) (shift' c d t2)
shift' c d (TmIf sp t1 t2 t3)   = TmIf sp (shift' c d t1) (shift' c d t2) (shift' c d t3)
shift' _ _ t                    = t

-- Performs a capture avoid substitution of s for j in the given term.
substitute :: VarIndex -> Term -> Term -> Term
substitute j s k@(TmVar _ i)
  | i == j    = s
  | otherwise = k
substitute j s (TmAbs sp n typ t1)  = TmAbs sp n typ $ substitute (j + 1) (shift 1 s) t1
substitute j s (TmApp sp t1 t2)     = TmApp sp (substitute j s t1) (substitute j s t2)
substitute j s (TmIf sp t1 t2 t3)   = TmIf sp (substitute j s t1) (substitute j s t2) (substitute j s t3)
substitute _ _ t                    = t
