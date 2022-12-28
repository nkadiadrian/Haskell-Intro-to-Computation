-- Informatics 1 Functional Programming
-- December 2017
-- SITTING 1 (09:30 - 11:30)

module Dec2017 where

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ), Gen, suchThat,
                        oneof, elements, sized, (==>) )
import Control.Monad -- defines liftM, liftM2, liftM3, used below
import Data.Char

-- Question 1

f :: [Int] -> [Int]
f xs = [ y - x | (x,y) <- zip xs (tail xs), x < y]
  
g :: [Int] -> [Int]
g [] = []
g [x] = []
g (x:y:xs) | y > x = (y - x) : g (y:xs)
           | otherwise = g (y:xs)

  
-- Question 2

-- 2a

isInitialism :: String -> Bool
isInitialism xs = length xs > 1 && and(map isUpper xs)

p :: [String] -> Int
p = sum . map length . filter isInitialism

-- 2b

isInitialism' :: String -> Bool
isInitialism' []    = False
isInitialism' [x]   = False
isInitialism' [x,y] | isUpper x && isUpper y = True
isInitialism' (x:xs)| isUpper x = isInitialism' xs 
                    | otherwise = False

prop_Initial xs = isInitialism xs == isInitialism' xs

q :: [String] -> Int
q [] = 0
q (x:xs) | isInitialism' x = length x + q xs
         | otherwise       = q xs

-- 2c

r :: [String] -> Int
r = sum . map length . filter isInitialism

-- Question 3

data Expr = X                      -- variable
          | Const Int              -- integer constant >=0
          | Expr :+: Expr          -- addition
          | Expr :*: Expr          -- multiplication
          deriving (Eq, Ord)

-- turns an Expr into a string approximating mathematical notation

showExpr :: Expr -> String
showExpr X          =  "X"
showExpr (Const n)  =  show n
showExpr (p :+: q)  =  "(" ++ showExpr p ++ "+" ++ showExpr q ++ ")"
showExpr (p :*: q)  =  "(" ++ showExpr p ++ "*" ++ showExpr q ++ ")"

-- For QuickCheck

instance Show Expr where
    show  =  showExpr

instance Arbitrary Expr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [ return X
                                       , liftM Const genPos ]
                 | otherwise  =  oneof [ return X
                                       , liftM Const genPos
                                       , liftM2 (:+:) subform2 subform2
                                       , liftM2 (:*:) subform2 subform2
                                       ]
                 where
                   subform2  =  expr (n `div` 2)
                   genPos  =  oneof [ return 0, return 1, return 2, return 3, return 4,
                                      return 5, return 6, return 7, return 8, return 8 ]

-- 3a

eval :: Expr -> Int -> Int
eval (Const x) g = x
eval X g = g
eval (x :+: y) g = eval x g + eval y g
eval (x :*: y) g = eval x g * eval y g

-- 3b

isSimple :: Expr -> Bool
isSimple (Const x :*: y) = False
isSimple (Const x) = True
isSimple X = True
isSimple (x :+: y) = isSimple x && isSimple y 
isSimple (x :*: y) = isSimple x && isSimple y 

-- 3c

repeatPls :: Expr -> Expr -> Expr
repeatPls (Const n) e = foldr (:+:) e (tail (replicate n e))

simplify :: Expr -> Expr
simplify (Const 0 :*: e) = Const 0
simplify (Const 1 :*: e) = simplify e
simplify (Const n :*: e) = simplify (repeatPls (Const n) e)
simplify X = X
simplify (Const x) = Const x
simplify (x :*: y) = simplify (simplify x) :*: (simplify y)
simplify (x :+: y) = simplify (simplify x) :+: (simplify y)

test3c =
    simplify ((X :*: Const 3) :+: (Const 0 :*: X)) == (X :*: Const 3) :+: Const 0
    && simplify (X :*: (Const 3 :+: Const 4)) == X :*: (Const 3 :+: Const 4)
    && (simplify (Const 4 :+: (Const 3 :*: X)) == Const 4 :+: (X :+: (X :+: X))
    || simplify (Const 4 :+: (Const 3 :*: X)) == Const 4 :+: ((X :+: X) :+: X))
    && simplify (((Const 1 :*: Const 2) :*: (X :+: Const 1)) :*: Const 2) ==
    ((X :+: Const 1) :+: (X :+: Const 1)) :*: Const 2

prop1_simplify :: Expr -> Bool
prop1_simplify p = isSimple (simplify p)

prop2_simplify :: Expr -> Int -> Bool
prop2_simplify p i = eval p i == eval (simplify p) i    