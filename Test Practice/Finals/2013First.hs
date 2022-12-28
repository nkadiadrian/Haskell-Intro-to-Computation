-- Informatics 1 Functional Programming - Only the longer question (3)
-- December 2013
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>)  )
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char

-- Question 1

-- 1a

f :: String -> Int
f = undefined

-- 1b

g :: String -> Int
g = undefined


-- Question 2

-- 2a

p :: [Int] -> Bool
p = undefined

-- 2b

q :: [Int] -> Bool
q = undefined

-- 2c

r :: [Int] -> Bool
r = undefined

-- Question 3

data Expr = X
          | Const Int
          | Neg Expr
          | Expr :+: Expr
          | Expr :*: Expr
          deriving (Eq, Ord)

-- turns an Expr into a string approximating mathematical notation

showExpr :: Expr -> String
showExpr X          =  "X"
showExpr (Const n)  =  show n
showExpr (Neg p)    =  "(-" ++ showExpr p ++ ")"
showExpr (p :+: q)  =  "(" ++ showExpr p ++ "+" ++ showExpr q ++ ")"
showExpr (p :*: q)  =  "(" ++ showExpr p ++ "*" ++ showExpr q ++ ")"

-- evaluate an Expr, given a value of X

evalExpr :: Expr -> Int -> Int
evalExpr X v          =  v
evalExpr (Const n) _  =  n
evalExpr (Neg p) v    =  - (evalExpr p v)
evalExpr (p :+: q) v  =  (evalExpr p v) + (evalExpr q v)
evalExpr (p :*: q) v  =  (evalExpr p v) * (evalExpr q v)

-- For QuickCheck

instance Show Expr where
    show  =  showExpr

instance Arbitrary Expr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [elements [X]]
                 | otherwise  =  oneof [ liftM Const arbitrary
                                       , liftM Neg subform
                                       , liftM2 (:+:) subform subform
                                       , liftM2 (:*:) subform subform
                                       ]
                 where
                   subform  =  expr (n `div` 2)

-- 3a

rpn :: Expr -> [String]
rpn X         = ["X"]
rpn (Const c) = [show c]
rpn (x :*: y) = (rpn x) ++ (rpn y) ++ ["*"]
rpn (x :+: y) = (rpn x) ++ (rpn y) ++ ["+"]
rpn (Neg x)   = (rpn x) ++ ["-"] 

-- 3 b

the :: [String] -> String
the [x] = x
the xs = error "ill-formed RPN"

evalrpn :: [String] -> Int -> Int
evalrpn xs g = solve xs [] g


solve  :: [String] -> [String] -> Int -> Int
solve [] ys g = (read (the ys)::Int)
solve ("X":xs) ys g       = solve xs ((show g) : ys) g
solve ("-":xs) (y:ys)   g = solve xs (show ((read y::Int) * (-1)) : ys) g
solve ("-":xs) []       g = error "ill-formed RPN"
solve ("*":xs) (w:y:ys) g = solve xs (show ((read w::Int) * (read y::Int)) : ys) g
solve ("*":xs) []       g = error "ill-formed RPN"
solve ("*":xs) [a]      g = error "ill-formed RPN"
solve ("+":xs) (w:y:ys) g = solve xs (show ((read w::Int) + (read y::Int)) : ys) g
solve ("+":xs) []       g = error "ill-formed RPN"
solve ("+":xs) [a]      g = error "ill-formed RPN"
solve (x:xs)   ys       g = solve xs (x:ys) g


--evalrpn (x:"-":xs)   g = evalrpn (show ((evalrpn [x] g) * (-1)) : xs) g
--evalrpn (x:y:"-":xs)   g = evalrpn ( [x] ++ (show ((evalrpn [y] g) * (-1)) : xs)) g
--evalrpn (x:y:"*":xs) g = evalrpn (show ((evalrpn [x] g) * (evalrpn [y] g)) : xs) g
--evalrpn (x:y:"+":xs) g = evalrpn (show ((evalrpn [x] g) + (evalrpn [y] g)) : xs) g
--evalrpn ["X"] g = g
--evalrpn x g | and $ map (\y -> isDigit y || y == '-') (the x)  = read (the x) :: Int
  --          | otherwise = error "Invalinput"--