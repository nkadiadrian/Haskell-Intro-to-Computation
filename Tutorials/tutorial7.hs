-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 7
--
-- Week 7(29 Oct.- 01 Nov.)

-- module Main where

import LSystem
import Test.QuickCheck

pathExample = (Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#:  Go 30)

-- 1a. split
split :: Command -> [Command]
split Sit = []
split (p :#: q) = (split p) ++ (split q)
split p = [p]

-- 1b. join
join :: [Command] -> Command
join [p] = p
join (p:q) = p :#: (join q)


-- 1c. equivalent
-- equivalent 
equivalent :: Command -> Command -> Bool
equivalent p q = (split p) == (split q)

-- 1d. testing join and split
-- prop_split_join 
prop_split_join :: Command -> Bool
prop_split_join c = equivalent (join (split c)) c

-- prop_split
prop_split :: Command -> Bool
prop_split c = all conSplit (split c)
    where
        conSplit Sit = False
        conSplit (p :#: q) = False
        conSplit _ = True


-- 2a. copy
copy :: Int -> Command -> Command
copy n p | n <  1 = Sit
         | n == 1 = p
         | n >= 2 = p :#: (copy (n-1) p)

copy' n p = join (replicate n p)

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon a = copy 5 (Go a :#: Turn 72)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon d 0 = Sit
polygon d 1 = Sit
polygon d sides = copy sides (Go d :#: Turn (36- fromIntegral( sides)))


-- 3. spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral side n step angle | n > 0 && side > 0 = (Go side :#: Turn angle) :#: spiral (side + step) (n-1) step angle
                         | otherwise = Sit


-- 4. optimise
-- Remember that Go does not take negative arguments.

optimise :: Command -> Command
optimise = join.opt.filter(/= Turn 0).opt.filter(/= Go 0).split
    where
        opt [] = []
        opt (Go x: Go y: xs) = opt (Go (x + y) : xs)
        opt (Turn x : Turn y: xs) = opt (Turn (x + y): opt xs)
        opt (c : cs) = c : opt cs



-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead = undefined


-- 6. snowflake
snowflake :: Int -> Command
snowflake = undefined


-- 7. hilbert
hilbert :: Int -> Command
hilbert = undefined

--------------------------------------------------
--------------------------------------------------
---------------- Optional Material ---------------
--------------------------------------------------
--------------------------------------------------

-- Bonus L-Systems

peanoGosper = undefined


cross = undefined


branch = undefined

thirtytwo = undefined

main :: IO ()
main = display pathExample