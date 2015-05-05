{- CS320 Fall 2013
 - Weston Vial
 - Module1.hs -}

module Module1 where

import AbstractSyntax

f1 env (Value T    ) = Just TyBoolean
f1 env (Value (N n)) = Just TyNumber
f1 env (Variable x ) = lookup x env
f1 env (Plus  e1 e2) = if f1 env e1 == Just TyNumber && f1 env e2 == Just TyNumber then Just TyNumber else Nothing
f1 env (And   e1 e2) = if f1 env e1 == Just TyBoolean && f1 env e2 == Just TyBoolean then Just TyBoolean else Nothing
f1 env (Not   e    ) = if f1 env e == Just TyBoolean then Just TyBoolean else Nothing

f2 env (Assign x e s) =
  if   f1 env e `elem` [Just TyNumber, Just TyBoolean] 
    && f2 ((x, (\(Just t) -> t) $ f1 env e):env) s == Just TyVoid
  then 
    Just TyVoid 
  else
    Nothing
f2 env (If e s1 s2) =
  if   f1 env e `elem` [Just TyBoolean] 
    && f2 env s1 == Just TyVoid
    && f2 env s2 == Just TyVoid
  then 
    Just TyVoid 
  else
    Nothing
f2 env (End) = Just TyVoid

--eof
