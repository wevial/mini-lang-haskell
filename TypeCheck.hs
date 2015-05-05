{- CS320 Fall 2013
 - Weston Vial
 - TypeCheck.hs -}

module TypeCheck where

import AbstractSyntax
import Parse

class Typeable a where
  ty :: [(String, Type)] -> a -> Maybe Type

instance Typeable Exp where
  ty env (Value (N n)) = Just TyNumber
  ty env (Value _    ) = Just TyBoolean
  ty env (Variable x ) = lookup x env
  ty env (Plus  e1 e2) = if ty env e1 == Just TyNumber && ty env e2 == Just TyNumber then Just TyNumber else Nothing
  ty env (Minus e1 e2) = if ty env e1 == Just TyNumber && ty env e2 == Just TyNumber then Just TyNumber else Nothing
  ty env (Mult  e1 e2) = if ty env e1 == Just TyNumber && ty env e2 == Just TyNumber then Just TyNumber else Nothing
  ty env (And   e1 e2) = if ty env e1 == Just TyBoolean && ty env e2 == Just TyBoolean then Just TyBoolean else Nothing
  ty env (Or    e1 e2) = if ty env e1 == Just TyBoolean && ty env e2 == Just TyBoolean then Just TyBoolean else Nothing
  ty env (Not   e    ) = if ty env e == Just TyBoolean then Just TyBoolean else Nothing

instance Typeable Stmt where
  ty env (Assign x e s) =
    if ty env e `elem` [Just TyNumber, Just TyBoolean] 
        && ty ((x, (\(Just t) -> t) $ ty env e):env) s == Just TyVoid
      then Just TyVoid 
      else Nothing
  ty env (Print e s) =
    if ty env e `elem` [Just TyNumber, Just TyBoolean]
        && ty env s == Just TyVoid
      then Just TyVoid
  	else Nothing
  ty env (If e s1 s2) =
    if ty env e `elem` [Just TyBoolean] 
        && ty env s1 == Just TyVoid
        && ty env s2 == Just TyVoid
      then Just TyVoid 
      else Nothing
  ty env (End) = Just TyVoid
  ty _ _ = Nothing

typeCheck :: (Eq a, Parseable a, Typeable a) => a -> Maybe a
typeCheck pt = if ty [] pt == Nothing
  then Nothing
  else Just pt

--eof
