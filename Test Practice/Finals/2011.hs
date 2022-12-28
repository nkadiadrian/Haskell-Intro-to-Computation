-- Informatics 1 Functional Programming - Only the longer question (3)
-- December 2011

import Test.QuickCheck
import Control.Monad -- defines liftM, liftM2, used below

-- 1

-- 1a

f :: [Int] -> Int
f =  undefined

-- 1b

g :: [Int] -> Int
g =  undefined

-- 2

-- 2a

p :: [Int] -> Int
p =  undefined

-- 2b

q :: [Int] -> Int
q =  undefined

-- 2c

r :: [Int] -> Int
r =  undefined

-- 3

data Expr = Var String
          | Expr :+: Expr
          | Expr :*: Expr
          deriving (Eq, Show)

-- code that enables QuickCheck to generate arbitrary values of type Expr

instance Arbitrary Expr where
  arbitrary = sized arb
    where
    arb 0          =  liftM Var arbitrary
    arb n | n > 0  =  oneof [liftM Var arbitrary,
                             liftM2 (:+:) sub sub, 
                             liftM2 (:*:) sub sub] 
      where
      sub = arb (n `div` 2)

-- 3a

isNorm :: Expr -> Bool
isNorm (Var x) = True
isNorm (x :+: y) = isNorm x && isNorm y
isNorm (x :*: y) = isTerm x && isTerm y 

isTerm :: Expr -> Bool
isTerm (Var x) = True
isTerm (x :+: y) = False
isTerm (x :*: y) = isTerm x && isTerm y

testNorm = isNorm (Var "x") == True &&
    isNorm (Var "x" :*: Var "y" :*: Var "z") == True &&
    isNorm ((Var "x" :*: Var "y") :+: Var "z") == True &&
    isNorm (Var "x" :*: (Var "y" :+: Var "z")) == False && 
    isNorm ((Var "x" :*: Var "y") :+: (Var "x" :*: Var "z")) == True &&
    isNorm ((Var "u" :+: Var "v") :*: (Var "x" :+: Var "y")) == False &&
    isNorm (((Var "u" :*: Var "x") :+: (Var "u" :*: Var "y")) :+: ((Var "v" :*: Var "x") :+: (Var "v" :*: Var "y"))) == True

-- 3b

norm :: Expr -> Expr
norm (Var x) = Var x
norm (a :+: b) = norm a :+: norm b
norm (a :*: b) = norm a *** norm b
  where
    (a :+: b) *** c = (a *** c) :+: (b *** c)
    a *** (b :+: c) = (a *** b) :+: (a *** c)
    a *** b         = a :*: b


--norm (x :*: (y :+: z)) = norm (norm (norm x :*: norm y) :+: norm (norm x :*: norm z))
--norm ((x :+: y) :*: z) = norm (norm (norm x :*: norm z) :+: norm (norm y :*: norm z))
--norm (x :*: y) = (norm x :*: norm y)
--norm (x :+: y) = (norm x :+: norm y)

prop_norm :: Expr -> Bool
prop_norm xs = isNorm (norm xs)

example1 = ((Var "x" :+: Var "y") :*: (Var "w" :*: Var "z")) :*: (Var "a" :+: (Var "b" :+: Var "c"))