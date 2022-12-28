-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 3
--
-- Week 3(30-04 Oct.)
module Tutorial3 where

import Data.Char
import Data.List
import Test.QuickCheck

import Data.Function
import Data.Maybe

-- 1.

halveEvensRec :: [Int] -> [Int]
halveEvensRec []                      = []
halveEvensRec (x:xs) | x `mod` 2 == 0 = div x 2 : halveEvensRec xs
                     | otherwise      = halveEvensRec xs

halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensRec xs


-- 2.

inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec lo hi []                          = []
inRangeRec lo hi (x:xs) | x >= lo && x <= hi = x : inRangeRec lo hi xs
                        | otherwise          = inRangeRec lo hi xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRange lo hi xs == inRangeRec lo hi xs


-- 3.

countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs) | x > 0 = posiCount + 1
                         | otherwise = countPositivesRec xs
                             where posiCount = countPositivesRec xs

countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

prop_countPositives :: [Int] -> Bool
prop_countPositives l = countPositives l == countPositivesRec l


-- 4.

multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs) | isDigit x = digitCount * digitToInt x
                     | otherwise = multDigitsRec xs 
                        where digitCount = multDigitsRec xs

multDigits :: String -> Int
multDigits str = product [digitToInt ch | ch <- str, isDigit ch]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs == multDigitsRec xs


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- Ceasar Cipher Exercises
-- =======================


-- 5.

lookUp :: Char -> [(Char, Char)] -> Char
lookUp ch xs | [y | (x,y) <- xs, x == ch] == [] = ch
             | otherwise = head[y | (x, y) <- xs, x == ch]

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec ch [] = ch
lookUpRec ch ((x,y):xs) | ch == x = y
                        | otherwise = lookUpRec ch xs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c k = lookUp c k == lookUpRec c k


-- 6.

encipher :: Int -> Char -> Char
encipher k ch = chr(mod (ord ch - 65 + k) 26 + 65)



-- 7.

normalize :: String -> String
normalize str = [toUpper x | x <- str, isAlpha x || isDigit x] 


encipherStr :: Int -> String -> String
encipherStr k str = [encipher k ch | ch <- normalize str]


-- 8.

reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs = [(y,x) | (x,y) <- xs]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec ((x,y):xs) = (y,x) : reverseKeyRec xs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey xs = reverseKey xs == reverseKeyRec xs


-- 9.

decipher :: Int -> Char -> Char
decipher k ch = chr(mod (ord ch - 65 - k) 26 + 65)

decipherStr :: Int -> String -> String
decipherStr k str = [decipher k ch | ch <- str, isAlpha ch || isDigit ch || ch == ' ']

decipherStrRec :: Int -> String -> String
decipherStrRec k [] = []
decipherStrRec k (x:xs) | isUpper x = x : decipherStrRec k xs
                        | isAlpha x = x : decipherStrRec k xs
                        | x == ' '  = x : decipherStrRec k xs
                        | otherwise = decipherStrRec k xs

-- Optional Material
-- =================


-- 10.

contains :: String -> String -> Bool
contains (x:xs) (y:ys) | x == y = xs == take (length xs) ys
                       | otherwise = contains (x:xs) ys

-- 11.

--candidates :: String -> [(Int, String)]
--candidates xs | y > 26 = []
  --            | contains "THE" || "AND" decipherStrRec y xs = goThrough ys
    --          | otherwise = candidates xs (y:ys) = ys
      --            where (y:ys) = [0..26],
        --          goThrough 


-- 12.

splitEachFive :: String -> [String]
splitEachFive = undefined

prop_transpose :: String -> Bool
prop_transpose = undefined


-- 13.
encrypt :: Int -> String -> String
encrypt = undefined


-- 14.
decrypt :: Int -> String -> String
decrypt = undefined
