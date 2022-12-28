-- Informatics 1 Functional Programming - Only the longer question (3)
-- December 2012
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )
import Control.Monad -- defines liftM, liftM2, used below

-- Question 1

-- 1a

f :: Int -> [Int] -> [Int]
f =  undefined

-- 1b

g :: Int -> [Int] -> [Int]
g =  undefined

-- Question 2

-- 2a

p :: [Int] -> Bool
p =  undefined

-- 2b

q :: [Int] -> Bool
q =  undefined

-- 2c

r :: [Int] -> Bool
r =  undefined

-- Question 3

data Prop = X
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          | Prop :<->: Prop
          deriving (Eq, Ord)

-- turns a Prop into a string approximating mathematical notation

showProp :: Prop -> String
showProp X          =  "X"
showProp F          =  "F"
showProp T          =  "T"
showProp (Not p)    =  "(~" ++ showProp p ++ ")"
showProp (p :|: q)  =  "(" ++ showProp p ++ "|" ++ showProp q ++ ")"
showProp (p :<->: q)  =  "(" ++ showProp p ++ "<->" ++ showProp q ++ ")"

-- For QuickCheck

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:|:) subform subform
                                       ]
                 where
                   atom = oneof [elements [X,F,T]]
                   subform  =  prop (n `div` 2)

-- 3a

eval :: Prop -> Bool -> Bool
eval T b = True
eval F b = False
eval X b = b
eval (Not y) b = not (eval y b)
eval (x :|: y) b = (eval x b) || (eval y b)
eval (x :<->: y) b = (eval x b) == (eval y b)

-- 3b

simplify :: Prop -> Prop
simplify X = X
simplify F = F
simplify T = T
simplify (Not p) = negate (simplify p)
  where
    negate T = F
    negate F = T
    negate (Not p) = p
    negate p = Not p
simplify (p :|: q) = disjoin (simplify p) (simplify q)
  where
    disjoin T p = T
    disjoin F p = p
    disjoin p T = T
    disjoin p F = p
    disjoin p q | p == q = p
                | otherwise = p :|: q

prop_3 p =
    eval p True == eval (simplify p) True
    && eval p False == eval (simplify p) False
    && length (showProp p) >= length (showProp (simplify p))
    