import Data.Char
import Data.Function
import Test.QuickCheck

count :: String -> Int
count xs = length[ x | x <- xs, isUpper x || isDigit x]

countRec :: String -> Int
countRec [] = 0
countRec (x:xs) | isDigit x || isUpper x = 1 + countRec xs
                | otherwise = countRec xs

prop_count :: String -> Bool
prop_count xs = count xs == countRec xs

isNext :: Int -> Int -> Bool
isNext x y | even x = (div x 2) == y
           | odd  x = y == (x * 3) + 1
           | otherwise = error "Why"

collatz :: [Int] -> Bool
collatz xs = and[ isNext x (xs!!(y+1)) | (x, y) <- zip xs [0..], (y +1) < length xs ]

collatzRec :: [Int] -> Bool
collatzRec [] = True
collatzRec [x] = True
collatzRec (x:xs) | isNext x (head xs) = collatzRec xs
                  | otherwise = False