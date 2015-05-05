{- CS320 Fall 2013
 - Weston Vial
 - Module3.hs -}

module Module3 where

import AbstractSyntax

vAnd T T = T
vAnd _ _ = F

vOr F F = F
vOr _ _ = T

vNot T = F
vNot F = T

f3 env (Value v    ) = v
f3 env (Variable x ) = head [v | (y,v) <- env, y == x]
f3 env (Plus  e1 e2) = (\(N x) (N y) -> N (x + y)) (f3 env e1) (f3 env e2)
f3 env (Or    e1 e2) = vOr (f3 env e1) (f3 env e2)
f3 env (Not   e    ) = vNot (f3 env e)

f4 env1 (End         ) = (env1, [])
f4 env1 (Assign x e s) = let v = f3 env1 e in f4 ((x,v):env1) s
f4 env1 (If   e s1 s2) =
  if f3 env1 e == F then 
    f4 env1 s2
  else
    let (env2, o1) = f4 env1 s1 
        (env3, o2) = f4 env2 s2
    in (env3, o1 ++ o2)

-- eof
