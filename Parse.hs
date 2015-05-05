{- CS320 Fall 2013
 - Weston Vial
 - Parse.hs -}

module Parse where

import AbstractSyntax

type Token = String

-- This parser is a predictive parser.

-- NOTE: The (list!!n) operation retrieves the nth element in a list.

tokenize :: String -> [Token]
tokenize s =
  let splits = [0] ++ concat [[i,i+1] | i <- [0..length s-1], s!!i `elem` "{}(), "] ++ [length s]
      all = [ [s!!i | i <- [splits!!k..(splits!!(k+1)-1)]] | k <- [0..length splits-2] ]
  in [token | token <- all, token /= " " && token /= ""]

class Parseable a where
  parse :: [Token] -> Maybe (a, [Token])

instance Parseable Exp where
  parse (t:ts) =
    if t == "plus" && length ts > 0 && ts!!0 == "(" then
      let r = parse (tail ts)
      in if r == Nothing then Nothing else
          let Just (e1, ts) = r
          in if length ts == 0 || ts!!0 /= "," then Nothing else
              let r = parse (tail ts)
              in if r == Nothing then Nothing else
                  let Just (e2, ts) = r
                  in if ts!!0 /= ")" then Nothing else
                      Just (Plus e1 e2, tail ts)
    else if t == "minus" && length ts > 0 && ts!!0 == "(" then
      let r = parse (tail ts)
      in if r == Nothing then Nothing else
          let Just (e1, ts) = r
          in if length ts == 0 || ts!!0 /= "," then Nothing else
              let r = parse (tail ts)
              in if r == Nothing then Nothing else
                  let Just (e2, ts) = r
                  in if ts!!0 /= ")" then Nothing else
                      Just (Minus e1 e2, tail ts)
    else if t == "mult" && length ts > 0 && ts!!0 == "(" then
      let r = parse (tail ts)
      in if r == Nothing then Nothing else
          let Just (e1, ts) = r
          in if length ts == 0 || ts!!0 /= "," then Nothing else
              let r = parse (tail ts)
              in if r == Nothing then Nothing else
                  let Just (e2, ts) = r
                  in if ts!!0 /= ")" then Nothing else
                      Just (Mult e1 e2, tail ts)
    else if t == "and" && length ts > 0 && ts!!0 == "(" then
      let r = parse (tail ts)
      in if r == Nothing then Nothing else
          let Just (e1, ts) = r
          in if length ts == 0 || ts!!0 /= "," then Nothing else
              let r = parse (tail ts)
              in if r == Nothing then Nothing else
                  let Just (e2, ts) = r
                  in if ts!!0 /= ")" then Nothing else
                      Just (And e1 e2, tail ts)
    else if t == "or" && length ts > 0 && ts!!0 == "(" then
      let r = parse (tail ts)
      in if r == Nothing then Nothing else
          let Just (e1, ts) = r
          in if length ts == 0 || ts!!0 /= "," then Nothing else
              let r = parse (tail ts)
              in if r == Nothing then Nothing else
                  let Just (e2, ts) = r
                  in if ts!!0 /= ")" then Nothing else
                      Just (Or e1 e2, tail ts)
    else if t == "not" && length ts > 0 && ts!!0 == "(" then
      let r = parse (tail ts)
      in if r == Nothing then Nothing else
          let Just (e, ts) = r
          in if length ts == 0 || ts!!0 /= ")" then Nothing else
              Just (Not e, tail ts)
    else if t == "true" then
      Just (Value T, ts)
    else if subset t "abcdefghijklmnopqrstuvwxyz" then
      Just (Variable t, ts)
    else if subset t "0123456789" then
      Just (Value (N (read t)), ts)
    else
      Nothing
  parse _ = Nothing

instance Parseable Stmt where
  parse (t:ts) =
    if t == "end" then
      Just (End, ts)
    else if t == "assign" && length ts > 1 && ts!!0 `subset` "abcdefghijklmnopqrstuvwxyz" && ts!!1 == ":=" then
      let x = ts!!0
          r = parse (drop 2 ts)
      in if r == Nothing then Nothing else
          let Just (e, ts) = r
              r' = parse ts
          in if r' == Nothing then Nothing else
              let Just (s, ts) = r'
              in Just (Assign x e s, ts)
    else if t == "print" then
      let r = parse ts
      in if r == Nothing then Nothing else
          let Just (e, ts) = r
              r' = parse ts
          in if r' == Nothing then Nothing else
              let Just (s, ts) = r'
              in Just (Print e s, ts)
    else if t == "if" then
      let r = parse ts
      in if r == Nothing then Nothing else
          let Just (e, ts) = r
          in if length ts == 0 || ts!!0 /= "{" then Nothing else
              let r' = parse (tail ts)
              in if r' == Nothing then Nothing else
                  let Just (s1, ts) = r'
                  in if length ts == 0 || ts!!0 /= "}" then Nothing else
                      let r'' = parse (tail ts)
                      in if r'' == Nothing then Nothing else
                          let Just (s2, ts) = r''
                          in Just (If e s1 s2, ts)
    else
      Nothing
  parse _ = Nothing

-- Useful helper functions.
subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = and [x `elem` ys | x <- xs]
  
tokenizeParse :: (Eq a, Parseable a) => String -> Maybe a
tokenizeParse s = let t = parse (tokenize s) in
  if t == Nothing
     then Nothing
     else unwrapFst t

-- Helper function for tokenizeParse
unwrapFst :: Maybe (a, b) -> Maybe a
unwrapFst (Just (x, _)) = Just x

--eof
