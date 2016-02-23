module TypeChecker ( typeOf
                   ) where


import AST

-- TODO:
--  [ ] Improve type error messages.
typeOf :: Term -> Context -> Type
typeOf (TmTrue) ctx         = TyBool
typeOf (TmFalse) ctx        = TyBool
typeOf (TmIf t1 t2 t3) ctx  =
  let t1Type  = typeOf t1 ctx
      t2Type  = typeOf t2 ctx
      t3Type  = typeOf t3 ctx
  in  if t1Type == TyBool
        then if t2Type == t3Type
              then t2Type
              else error "Arms of conditional have different types."
        else error "Conditional guard not a boolean."
typeOf (TmVar i) ctx        = undefined
typeOf (TmAbs n typ t1) ctx = undefined
typeOf (TmApp t1 t2) ctx    =
  let t1Type  = typeOf t1 ctx
      t2Type  = typeOf t2 ctx
  in  case t1Type of
        (TyArrow t11Type t12Type) -> if t11Type == t2Type
                                      then t12Type
                                      else error "Parameter type mismatch."
        _                         -> error "Left side of application is not a function."
