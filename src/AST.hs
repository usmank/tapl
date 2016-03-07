module AST where

import Data.Functor.Identity
import Data.List
import Data.Maybe (fromMaybe)
import Text.Parsec

data Term = TmVar SourcePos VarIndex
          | TmAbs SourcePos VarName Type Term
          | TmApp SourcePos Term Term
          | TmTrue SourcePos
          | TmFalse SourcePos
          | TmIf SourcePos Term Term Term
          deriving (Eq)

data Type = TyBool
          | TyArrow Type Type
          deriving (Eq)

data VarBinding = NameBind
                | VarBind Type
                deriving (Eq, Show)

type VarIndex = Integer

type VarName = String

type Context = [(VarName, VarBinding)]

type Parser = ParsecT String Context Identity

instance Show Term where
  show = showTerm emptyContext

instance Show Type where
  show (TyBool)                       = "Bool"
  show (TyArrow t1@(TyArrow _ _) t2)  = "(" ++ show t1 ++ ")->" ++ show t2
  show (TyArrow t1 t2)                = show t1 ++ "->" ++ show t2

emptyContext :: Context
emptyContext = []

-- Add the given variable name and binding to the context. Returns the updated context.
addBinding :: VarName -> VarBinding -> Context -> Context
addBinding n b ctx = (n, b):ctx

-- Add the given variable name to the context. Returns the updated context.
addVar :: VarName -> Context -> Context
addVar n ctx = addBinding n NameBind ctx

-- Gets the binding for the given index from the context. Basically a safer version of the partial function (!!).
getBinding :: VarIndex -> Context -> Maybe (VarName, VarBinding)
getBinding _ []     = Nothing
getBinding i ctx
  | i < 0           = Nothing
  | i > ctx_length  = Nothing
  | otherwise       = Just $ genericIndex ctx i
  where
    ctx_length = toInteger $ length ctx

-- Gets the de Bruijn index of the variable in the given context. In the case of diplicates returns the most closely
-- bound index (i.e. leftmost in the list).
getVarIndex :: VarName -> Context -> Maybe VarIndex
getVarIndex n ctx = fmap toInteger $ findIndex ((n ==) . fst) ctx

-- Gets the variable name associated with the given de Bruijn index in the given context.
getVarName :: VarIndex -> Context -> Maybe VarName
getVarName i ctx = fmap fst $ getBinding i ctx

-- Get the type of variable in the context.
getVarType :: VarIndex -> Context -> Maybe Type
getVarType i ctx =
  case fmap snd (getBinding i ctx) of
    Just (VarBind typ)  -> Just typ
    _                   -> Nothing

-- Given a list of terms, create a string representation of the terms with each term terminated by a semicolon and
-- newline.
showTerms :: [(Term, Type)] -> String
showTerms = intercalate "\n" . fmap f
  where
    f t = concat [show (fst t), " : ", show (snd t), ";"]

-- Given a context and a term, create the string representation of the term.
showTerm :: Context -> Term -> String
showTerm ctx (TmVar sp i)       = fromMaybe (error $ "Could not find " ++ show i ++ " in context.") (getVarName i ctx)
showTerm ctx (TmAbs _ n typ t1) =
  let (n', ctx') = pickUniqueName n ctx
  in  "(λ" ++ n' ++ ":" ++ show typ ++ "." ++ showTerm ctx' t1 ++ ")"
showTerm ctx (TmApp _ t1 t2)    = showTerm ctx t1 ++ " " ++ showTerm ctx t2
showTerm ctx (TmTrue _)         = "true"
showTerm ctx (TmFalse _)        = "false"
showTerm ctx (TmIf _ t1 t2 t3)  = "if " ++ showTerm ctx t1 ++ " then " ++ showTerm ctx t2 ++ " else " ++ showTerm ctx t3

-- Pick a variable name that is unique within the given context. Appends single quotes to the given name until a unique
-- name is found. Returns both the unique name and a new context which contains the newly created name.
pickUniqueName :: VarName -> Context -> (VarName, Context)
pickUniqueName n ctx
  | isNameInContext n ctx = pickUniqueName (n ++ "\'") ctx
  | otherwise             = (n, addVar n ctx)

-- Return true if the given variable name is an element of the given context. Ignore variable bindings.
isNameInContext :: VarName -> Context -> Bool
isNameInContext _ [] = False
isNameInContext n ((x, _):xs)
  | n == x    = True
  | otherwise = isNameInContext n xs
