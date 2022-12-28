import Data.List
import Data.Char
import Data.Maybe

type Day = Int
type Month = String
type Year = Int
type Date = (Day, Month, Year)

leapYear :: Year -> Bool
leapYear x | mod x 400 == 0 = True
           | mod x 100 == 0 = False
           | mod x 4   == 0 = True
           | otherwise = False

daysInMonth :: Month -> Year -> Day            
daysInMonth x y | x `elem` ["Jan", "Mar", "May", "Jul", "Aug", "Oct", "Dec"] = 31
                | x `elem` ["Apr", "Jun", "Sep", "Nov"] = 30
                | x == "Feb" && (leapYear y) = 29
                | x == "Feb" && not (leapYear y) = 28
                | otherwise = 0

validDate :: Date -> Bool
validDate (d,m,y) = d >= 1 && d <= (daysInMonth m y)

showDate :: Date -> String
showDate (d,m,y) = (show d) ++ " " ++ m ++ " " ++ (show y)

type Point = (Float, Float)

dist :: Point -> Point -> Float
dist (x,y) (u,v) = sqrt (sqr (x-u) + sqr (y-v))
    where sqr x = x*x

totalComp :: [Point] -> Float
totalComp (x:xs) = sum [dist x y | y <- (x:xs)]

totalRec :: [Point] -> Float
totalRec [] = 0
totalRec [x] = 0
totalRec (x:y:xs) = (dist x y) + totalRec (x:xs)
