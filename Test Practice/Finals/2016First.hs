-- Informatics 1 Functional Programming - Only the longer question (3)
-- December 2016
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>), Property )
import Control.Monad -- defines liftM, liftM3, used below
import Data.List
import Data.Char

-- Question 1

-- 1a

f :: [Int] -> [Int] -> Int
f = undefined

-- 1b

g :: [Int] -> [Int] -> Int
g = undefined

-- Question 2

-- 2a

p :: String -> Int
p = undefined

-- 2b

q :: String -> Int
q = undefined

-- 2c

r :: String -> Int
r = undefined

-- Question 3

data Move =
     Go Int            -- move the given distance in the current direction
   | Turn              -- reverse direction
   | Dance             -- dance in place, without changing direction
  deriving (Eq,Show)   -- defines obvious == and show

data Command =
     Nil                      -- do nothing
   | Command :#: Move         -- do a command followed by a move
  deriving Eq                 -- defines obvious ==

instance Show Command where   -- defines show :: Command -> String
  show Nil = "Nil"
  show (com :#: mov) = show com ++ " :#: " ++ show mov

type Position = Int
data Direction = L | R
  deriving (Eq,Show)          -- defines obvious == and show
type State = (Position, Direction)

-- For QuickCheck

instance Arbitrary Move where
  arbitrary = sized expr
    where
      expr n | n <= 0 = elements [Turn, Dance]
             | otherwise = liftM (Go) arbitrary

instance Arbitrary Command where
  arbitrary = sized expr
    where
      expr n | n <= 0 = oneof [elements [Nil]]
             | otherwise = oneof [ liftM2 (:#:) subform arbitrary
                                 ]
             where
               subform = expr (n-1)

instance Arbitrary Direction where
  arbitrary = elements [L,R]

-- 3a

state :: Move -> State -> State
state (Go x) (y,z) = ((y+x),z)
state Turn   (x,L) = (x,R)
state Turn   (x,R) = (x,L)
state Dance  (x,y) = (x,y)

-- 3b

trace :: Command -> State -> [State]
trace (Nil) y = [y]
trace (x :#: y) z =  (trace x z) ++ [state y (last (trace x z))]

-- 3c

furtherpos :: State -> [State] -> Bool
furtherpos (p,s) ss = and [ abs p > abs q | (q,_) <- ss ]

dancify :: Command -> Command
dancify Nil = Nil
dancify (com :#: Dance) = (dancify com) :#: Dance
dancify (com :#: m) | furtherpos (state m (last t)) t = (dancify com) :#: m :#: Dance
                    | otherwise = (dancify com) :#: m
  where t = trace com (0,R)


quint (x,y) | x /= y = [x,y]
            | otherwise = [x]
