import Data.List
import Data.Char
import Data.Maybe

type Deg = Int
type Min = Int
type Angle = (Deg, Min)
type Longitude = (Angle, Char)

value :: Angle -> Float
value (d, m) = fromIntegral d + (fromIntegral m)/60

ok :: Longitude -> Bool
ok ((d,m), l) = and[d >= 0, d <= 180, m >= 0, m <= 59, (value (d,m)) <= 180, l == 'W' || l == 'E']

showLongitude :: Longitude -> String
showLongitude ((d,m), l) = show d ++ "." ++ show m ++ "'" ++ [l]

swap :: Char -> Char
swap x | isUpper x = toLower x
       | isLower x = toUpper x
       | otherwise = x

swaps :: String -> String
swaps xs = [swap x | x <- xs, isAlpha x]

swaps' :: String -> String
swaps' [] = []
swaps' (x:xs) | isAlpha x = swap x : swaps' xs
              | otherwise = swaps' xs

same :: (Eq a) => [a] -> Bool
same xs = or [x == x' | (x, x')  <- zip xs (tail xs)]

sameRec :: (Eq a) => [a] -> Bool
sameRec [] = False
sameRec [x] = False
sameRec (x:y:xs) | x == y = True
                 | otherwise = sameRec (y:xs)