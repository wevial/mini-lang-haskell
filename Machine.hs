{- CS320 Fall 2013
 - Weston Vial
 - Machine.hs -}

module Machine where

type Address = Int
type Location = Int
type Label = String
type Control = Int

type Memory = [Int]
type Output = [Int]

-- All arithmetic instructions in the language are applied
-- to memory addresses 1 and 2, and the result is stored
-- in memory addresss 0.

-- The Output instruction outputs the value at the specified
-- address to the output buffer (a list of outputs in the
-- simulator).

data Instruction =
    Label Label
  | Goto Label
  | Branch Address Label
  | Set Address Int
  | Copy Address Address
  | Add
  | Subtract
  | Multiply
  | Output Address
  deriving (Eq, Show)

addrs :: Instruction -> [Address]
addrs (Branch a _) = [a]
addrs (Set a _   ) = [a]
addrs (Copy a a' ) = [a,a']
addrs (Output a  ) = [a]
addrs _            = []

set :: Address -> Int -> Memory -> Memory
set addr n mem = take addr mem ++ [n] ++ drop (addr+1) mem

labelIndex :: Label -> [Instruction] -> Int
labelIndex label insts = head [index | (index, inst) <- zip [0..] insts, inst == Label label]

-- The (list!!n) operation retrieves the nth element in a list.

step :: Instruction -> ([Instruction], Control, Memory, Output) -> ([Instruction], Control, Memory, Output)
step (Label label      ) (insts, c, mem, o) = (insts, c+1, mem, o)
step (Goto label       ) (insts, c, mem, o) = (insts, labelIndex label insts, mem, o)
step (Branch addr label) (insts, c, mem, o) = (insts, if mem!!addr /= 0 then labelIndex label insts else c+1, mem, o)
step (Set addr n       ) (insts, c, mem, o) = (insts, c+1, set addr n mem, o)
step (Copy from to     ) (insts, c, mem, o) = (insts, c+1, set to (mem!!from) mem, o)
step (Add              ) (insts, c, mem, o) = (insts, c+1, set 0 (mem!!1 + mem!!2) mem, o)
step (Subtract         ) (insts, c, mem, o) = (insts, c+1, set 0 (mem!!1 - mem!!2) mem, o)
step (Multiply         ) (insts, c, mem, o) = (insts, c+1, set 0 (mem!!1 * mem!!2) mem, o)
step (Output addr      ) (insts, c, mem, o) = (insts, c+1, mem, (mem!!addr):o)

steps :: ([Instruction], Control, Memory, Output) -> ([Instruction], Control, Memory, Output)
steps (insts, c, mem, o) =
  if c < length insts then
    steps (step (insts!!c) (insts, c, mem, o))
  else
    (insts, c, mem, o)

simulate :: [Instruction] -> Output
simulate insts = (\(_,_,_,o) -> o) (steps (insts, 0, [0..maximum (concat (map addrs insts))], []))

--eof
