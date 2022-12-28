-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 2
--
-- Week 2(23-27 Sep.)

import Data.Char
import Data.List
import Test.QuickCheck
import Control.Monad (guard)


-- 1. halveEvens

myList :: [Int]
myList = [0,2,1,7,8,56,17,18]

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [div x 2 | x <- xs, even x]

-- This is for testing only. Do not try to understand this (yet).
halveEvensReference :: [Int] -> [Int]
halveEvensReference = (>>= \x -> guard (x `mod` 2 == 0) >>= \_ -> return $ x `div` 2)


-- -- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensReference xs

-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, x >= lo, x <= hi]


-- 3. countPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives list = length ([x | x <- list, x>0])


-- 4. multDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits str = product[digitToInt x | x <- str, isDigit x]

countDigits :: String -> Int
countDigits str = length[x | x <- str, isDigit x]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs <= 9 ^ (countDigits xs)


-- 5. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise []     = []
capitalise (x:xs) = toUpper x : [toLower x | x <- xs]


-- 6. title

lowercase :: String -> String
lowercase xs = [toLower x | x <- xs]

-- List-comprehension version

toCaseShift :: String -> String
toCaseShift xs | length xs > 3 = capitalise xs
               | otherwise = lowercase xs

title :: [String] -> [String]
title []     = []
title (x:xs) = capitalise x : [toCaseShift x | x <- xs]

-- 7. signs

sign :: Int -> Char
sign i | i >= 0 && i <= 9  = '+'
       | i <= 0 && i >= -9 = '-'
       | i == 0 = '0'
       | otherwise = error "not a solitary number"

signs :: [Int] -> String
signs xs = [sign x | x <- xs]


-- 8. score
isVowel :: Char -> Bool
isVowel x | toUpper x == 'A' = True
          | toUpper x == 'E' = True
          | toUpper x == 'I' = True
          | toUpper x == 'O' = True
          | toUpper x == 'U' = True
          | otherwise = False

score :: Char -> Int
score x | not (isLetter x) = 0
        | isUpper x && isVowel x = 3
        | isUpper x = 2
        | isVowel x = 2
        | otherwise = 1

totalScore :: String -> Int
totalScore xs = product[score x | x <- xs, isLetter x]

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = totalScore xs >= 1

-- Tutorial Activity
-- 10. pennypincher

-- List-comprehension version.
pennypincher :: [Int] -> Int
pennypincher prices = undefined

-- -- And the test itself
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = undefined

-- Optional Material

-- 11. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = undefined


-- 12. search

-- List-comprehension version

search :: String -> Char -> [Int]
search str goal = undefined

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = undefined


-- 13. contains

contains :: String -> String -> Bool
contains str substr = undefined

-- Depending on the property you want to test, you might want to change the type signature
prop_contains :: String -> String -> Bool
prop_contains str1 str2 = undefined
