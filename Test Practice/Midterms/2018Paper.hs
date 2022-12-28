import Data.Char
import Data.Function
import Test.QuickCheck

isVowel :: Char -> Bool
isVowel x = x `elem` "aeiouAEIOU"

m :: String -> Int
m xs = (2*length[x | x <- xs, isVowel x]) - (length xs)

n :: String -> Int
n [] = 0
n (x:xs) | isVowel x = 1 + n xs
         | otherwise = (-1) + n xs

f :: String -> Bool
f "" = True
f (x:xs) = and[ isAlpha a /= isAlpha (i) | (a,i) <- zip (x:xs) xs ]

g :: String -> Bool
g [] = True
g [x] = True
g (x:y:xs) | isAlpha x /= isAlpha y = g (y:xs)
           | otherwise = False

prop_fg :: String -> Bool
prop_fg xs = f xs ==  g xs