module AST where

import Data.Functor.Identity
import Data.List
import Data.Maybe (fromMaybe)
import Text.Parsec

data Term = TmVar VarIndex
          | TmAbs VarName Term
          | TmApp Term Term
          deriving (Eq)

instance Show Term where
  show = showTerm emptyNamingContext

type VarIndex = Integer

type VarName = String

type NamingContext = [VarName]

type Parser = ParsecT String NamingContext Identity

emptyNamingContext :: NamingContext
emptyNamingContext = []

-- Add the given variable name to the naming context. Returns the updated naming context.
addVar :: VarName -> NamingContext -> NamingContext
addVar = (:)

-- Get the de Bruijn index of the variable in the given naming context. In the case of diplicates it should return the
-- most closely bound index (i.e. leftmost in the list).
getVarIndex :: VarName -> NamingContext -> Maybe VarIndex
getVarIndex n ctx = fmap toInteger $ elemIndex n ctx

-- Get the variable name associated with the given de Bruijn index in the given naming context. Basically a safer
-- version of the partial function (!!).
getVarName :: VarIndex -> NamingContext -> Maybe VarName
getVarName _ [] = Nothing
getVarName i ctx
  | i < 0                         = Nothing
  | i > toInteger (length ctx)  = Nothing
  | otherwise                     = return $ genericIndex ctx i

-- Given a list of terms, create a string representation of the terms with each term terminated by a semicolon and
-- newline.
showTerms :: [Term] -> String
showTerms = intercalate "\n" . fmap ((++ ";") . show)

-- Given a naming context and a term, create the string representation of the term.
showTerm :: NamingContext -> Term -> String
showTerm ctx (TmVar i) = fromMaybe (error $ "Could not find " ++ show i ++ " in naming context.") (getVarName i ctx)
showTerm ctx (TmAbs n t1) =
  let (n', ctx') = pickUniqueName n ctx
  in  "(λ" ++ n' ++ "." ++ showTerm ctx' t1 ++ ")"
showTerm ctx (TmApp t1 t2) = showTerm ctx t1 ++ " " ++ showTerm ctx t2

-- Pick a variable name that is unique within the given naming context. Appends single quotes to the given name until a
-- unique name is found. Returns both the unique name and a new naming context which contains the newly created name.
pickUniqueName :: VarName -> NamingContext -> (VarName, NamingContext)
pickUniqueName n ctx
  | n `elem` ctx  = pickUniqueName (n ++ "\'") ctx
  | otherwise     = (n, n:ctx)
