module Core where

import Syntax

eval :: Term -> Term
eval t =
    let t' = eval' t
    in  if t == t' then t else eval t'

eval' :: Term -> Term
-- E-IfTrue
eval' (TmIf TmTrue t2 _) = t2
-- E-IfFalse
eval' (TmIf TmFalse _ t3) = t3
-- E-If
eval' (TmIf t1 t2 t3) = TmIf (eval t1) t2 t3
-- E-Succ
eval' (TmSucc t1) = TmSucc (eval' t1)
-- E-PredZero
eval' (TmPred TmZero) = TmZero
-- E-PredSucc
eval' (TmPred (TmSucc nv1))
    | isNumerical nv1 = nv1
-- E-Pred
eval' (TmPred t1) = TmPred (eval' t1)
-- E-IszeroZero
eval' (TmIsZero TmZero) = TmTrue
-- E-IszeroSucc
eval' (TmIsZero (TmSucc nv1))
    | isNumerical nv1 = TmFalse
-- E-Iszero
eval' (TmIsZero t1) = TmIsZero (eval' t1)
-- No rule applies, return the original term
eval' t = t

isNumerical :: Term -> Bool
isNumerical TmZero      = True
isNumerical (TmSucc t1) = isNumerical t1
isNumerical (TmPred t1) = isNumerical t1
isNumerical _           = False
