-- Informatics 1 Functional Programming - Only the longer question (3)
-- August 2014

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char

-- Question 1

-- 1a

f :: String -> String
f = undefined

-- 1b

g :: String -> String
g = undefined


-- Question 2

-- 2a

p :: [String] -> Int
p = undefined

-- 2b

q :: [String] -> Int
q = undefined

-- 2c

r :: [String] -> Int
r = undefined

-- Question 3

data Tree = Empty
          | Leaf Int
          | Node Tree Tree
        deriving (Eq, Ord, Show)

-- For QuickCheck

instance Arbitrary Tree where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [elements [Empty]]
                 | otherwise  =  oneof [ liftM Leaf arbitrary
                                       , liftM2 Node subform subform
                                       ]
                 where
                   subform  =  expr (n `div` 2)

-- For testing

t1 = Empty

t2 = Node (Leaf 1)
          Empty

t3 = Node (Node (Node (Leaf 3)
                      Empty)
                (Leaf 1))
          (Node Empty
                (Node (Leaf 3)
                      (Leaf 5)))

t4 = Node (Node (Node Empty
                      Empty)
                (Leaf 1))
          (Node Empty
                (Node Empty
                      Empty))

-- 3a

leafdepth :: Tree -> Int
leafdepth (Empty)  = 0
leafdepth (Leaf x) = 1
leafdepth (Node x y) = maximum [leafdepth x + 1, leafdepth y + 1]

-- 3 b

deepest1 :: Tree -> [Int]
deepest1 x = vibeCheck x (leafdepth x)

vibeCheck :: Tree -> Int -> [Int]
vibeCheck (Empty) g = []
vibeCheck (Leaf x) 1 = [x]
vibeCheck (Leaf x) y = []
vibeCheck (Node x y) num = vibeCheck x (num - 1) ++ vibeCheck y (num - 1)

-- 3c

deepest2 :: Tree -> [Int]
deepest2 Empty = []
deepest2 (Leaf x) = [x]
deepest2 (Node x y) | leafdepth x > leafdepth y = deepest2 x
                    | leafdepth y > leafdepth x = deepest2 y
                    | otherwise = deepest2 x ++ deepest2 y