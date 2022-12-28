import Data.Char
import Data.Function
import Test.QuickCheck

vowelly :: Int -> Bool
vowelly x | x >= 80 && x <= 89 = True
          | x == 18 || x == 8 || x == 1 || x == 11 = True
          | otherwise = False

count :: [Int] -> Int
count xs = length[x | x <- xs, vowelly x]   

countRec :: [Int] -> Int
countRec [] = 0
countRec (x:xs) | vowelly x = 1 + countRec xs
                | otherwise = countRec xs

c :: Char -> String -> String
c a str = [if even i then x else a | (x,i) <- zip str [0..]]

d :: Char -> String -> String
d x []  = []
d x [a] = [a]
d x (a:b:str) = a:x:d x str

prop_cd :: Char -> String -> Bool
prop_cd x xs = c x xs == d x xs