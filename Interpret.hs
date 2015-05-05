{- CS320 Fall 2013
 - Weston Vial
 - Interpret.hs -}


module Interpret where

import AbstractSyntax
import Parse
import TypeCheck

vAnd T T = T
vAnd _ _ = F

vOr F F = F
vOr _ _ = T

vNot T = F
vNot F = T

eval :: [(String, Val)] -> Exp -> Val
eval env (Value v    ) = v
eval env (Variable x ) = head [v | (y,v) <- env, y == x]
eval env (Plus  e1 e2) = (\(N x) (N y) -> N (x + y)) (eval env e1) (eval env e2)
eval env (Minus e1 e2) = (\(N x) (N y) -> N (x - y)) (eval env e1) (eval env e2)
eval env (Mult  e1 e2) = (\(N x) (N y) -> N (x * y)) (eval env e1) (eval env e2)
eval env (Or    e1 e2) = vOr (eval env e1) (eval env e2)
eval env (And   e1 e2) = vAnd (eval env e1) (eval env e2)
eval env (Not   e    ) = vNot (eval env e)

exec :: [(String, Val)] -> Stmt -> ([(String, Val)], Output)
exec env1 (End         ) = (env1, [])
exec env1 (Print    e s) =
  let o1 = [eval env1 e]
      (env2, o2) = exec env1 s
  in (env2, o1 ++ o2)
exec env1 (Assign x e s) = let v = eval env1 e in exec ((x,v):env1) s
exec env1 (If   e s1 s2) =
  if eval env1 e == F then 
    exec env1 s2
  else
    let (env2, o1) = exec env1 s1 
        (env3, o2) = exec env2 s2
    in (env3, o1 ++ o2)

interpret :: Stmt -> Output
interpret s = snd (exec [] s)

{- The following function never interprets a program that will cause an error
 - because errors in the program prior to being run. For starters, in the case
 - where the string is unparsable, it returns nothing prior to being executed.
 - It will also always only pass in statements into interpret by type checking
 - prior to execution (as expressions cannot be interpreted with this
 - interpreter). Both unbounded variables and unexhaustive patterns cannot get
 - parsed so this function returns Nothing and never runs the interpreter. -}
tokenizeParseTyChkInterpret :: String -> Maybe Output
tokenizeParseTyChkInterpret s = let pt = tokenizeParse s in
  if pt == Nothing || ty [] (unwrap pt) /= Just TyVoid
     then Nothing
  else Just (interpret (unwrap pt))
    
unwrap :: Maybe a -> a
unwrap (Just x) = x

-- eof
