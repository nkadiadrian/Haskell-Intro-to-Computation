-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 5
--
-- Week 5(14-18 Oct.)

module Tutorial5 where

import Data.Char
import Data.Ratio
import Test.QuickCheck

-- 1. Map

-- a.
doubles :: [Int] -> [Int]
doubles xs = map f xs 
    where       
        f x = 2*x

-- b.        
penceToPounds :: [Integer] -> [Float]
penceToPounds xs = map (\x -> (fromInteger x)/100.0) xs

-- c.
uppersComp :: String -> String
uppersComp xs = map f xs
        where
            f x = toUpper x

-- 2. Filter
-- a.
alphas :: String -> String
alphas xs = filter p xs
    where
        p x = isAlpha x

-- b.
above :: Int -> [Int] -> [Int]
above lowBound numbers = filter p numbers
    where
        p x = x > lowBound

-- c.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals xs = filter p xs
    where
        p (x,y) = x /= y 

-- d.
rmChar :: Char -> String -> String
rmChar a xs = filter p xs
    where
        p x = x /= a 

-- e.
rmCharComp :: Char -> String -> String
rmCharComp a xs = [x | x <- xs, x /= a]

prop_rmChar a xs = rmChar a xs == rmCharComp a xs
-- 3. Comprehensions vs. map & filter
-- a.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = map f (filter p xs)
        where
            p x = x > 3
            f x = 2*x

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- b.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' xs = map f (filter p xs)
    where
        p x = even (length x)
        f x = reverse x

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
andRec :: [Bool] -> Bool
andRec []     = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold xs = foldr (&&) True xs

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- b.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold xs = foldr (++) [] xs

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- c.
rmCharsRec :: String -> String -> String
rmCharsRec [] str = str
rmCharsRec (x:xs) str = rmChar x (rmCharsRec xs str)

rmCharsFold :: String -> String -> String
rmCharsFold xs str = foldr (rmChar) str xs

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str


type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform (x:xs) = all (== x) (x:xs)

-- b.
valid :: Matrix -> Bool
valid xs = uniform (map f xs)
    where
        f x = length x


-- 6.
matrixWidth :: Matrix -> Int
matrixWidth xs = head (map f xs)
    where
        f x = length x

matrixHeight :: Matrix -> Int
matrixHeight m = length(m)

plusRow :: [Rational] -> [Rational] -> [Rational]
plusRow xs ys | length xs == length ys = zipWith (+) xs ys
              | otherwise = error "This isn't possible with the given matrices' dimensions"

plusM :: Matrix -> Matrix -> Matrix
plusM xs ys | valid xs && valid ys = zipWith (zipWith (+)) xs ys
            | otherwise = error "This isn't possible with the given matrices' dimensions"

-- 7.
dotProd :: [Rational] -> [Rational] -> [Rational]
dotProd =  undefined

timesM :: Matrix -> Matrix -> Matrix
timesM = undefined

-- 8.
-- b.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [f (fst x) (snd x) | x <- zip xs ys]

-- c.
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = map (uncurry f) (zip xs ys)

prop_ZipWith f xs ys = zipWith' f xs ys == zipWith f xs ys
        where f x y = x*y

-- -----------------------------------
-- -----------------------------------
-- -- Optional material
-- -----------------------------------
-- -----------------------------------
-- -- 9.

-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = undefined

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f = undefined

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]     
removes = undefined

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined
        
determinant :: Matrix -> Rational
determinant = undefined

cofactors :: Matrix -> Matrix
cofactors m = undefined        
                
scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

prop_inverse2 :: Rational -> Rational -> Rational 
                -> Rational -> Property
prop_inverse2 a b c d = undefined

type Triple a = (a,a,a)
        
prop_inverse3 :: Triple Rational -> 
                 Triple Rational -> 
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = undefined