import Data.Char
import Data.Function
import Test.QuickCheck

f :: [Int] -> Int
f xs = sum[ x*x | x <- xs, mod (x) 3 == 0, mod (x) 5 /= 0]

g :: [Int] -> Int
g [] = 0
g (x:xs) | (mod (x) 3 == 0) && (mod (x) 5 /= 0) = (x*x) + g xs
         | otherwise = g xs

prop_fg :: [Int] -> Bool
prop_fg xs = g xs == f xs

mst :: Int -> Int -> Bool
mst x y | x >= 0 = y > 2*x
        | x <  0 = y > div x 2

ordered :: [Int] -> Bool
ordered xs = and[mst x (xs!!(i+1)) | (x,i) <- zip xs [0..], i+1 < length xs]

ordered' :: [Int] -> Bool
ordered' [] = True
ordered' [x] = True
ordered' (x:y:xs) | mst x y   = ordered' (y:xs)
                  | otherwise = False

prop_check xs = ordered' xs == ordered xs