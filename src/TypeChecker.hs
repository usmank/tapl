module TypeChecker ( typeOf
                   ) where

import Data.Maybe

import AST
import Util

typeOf :: Term -> Context -> Type
typeOf (TmTrue _) ctx         = TyBool
typeOf (TmFalse _) ctx        = TyBool
typeOf (TmIf sp t1 t2 t3) ctx  =
  let t1Type  = typeOf t1 ctx
      t2Type  = typeOf t2 ctx
      t3Type  = typeOf t3 ctx
  in  if t1Type == TyBool
        then if t2Type == t3Type
              then t2Type
              else err sp "Type error: Arms of conditional have different types."
        else err sp "Type error: Conditional guard not a boolean."
typeOf (TmVar sp i) ctx        = fromMaybe (err sp "Type error: Var type lookup failed.") (getVarType i ctx)
typeOf (TmAbs _ n typ t1) ctx =
  let ctx'    = addBinding n (VarBind typ) ctx
      t1Type  = typeOf t1 ctx'
  in  TyArrow typ t1Type
typeOf (TmApp sp t1 t2) ctx    =
  let t1Type  = typeOf t1 ctx
      t2Type  = typeOf t2 ctx
  in  case t1Type of
        (TyArrow t11Type t12Type) -> if t11Type == t2Type
                                      then t12Type
                                      else err sp "Type error: Parameter type mismatch."
        _                         -> err sp "Type error: Left side of application is not a function."
