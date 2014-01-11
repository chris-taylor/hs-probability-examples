module Dice where

import Control.Probability


die :: Dist Int
die = uniform [1..6]

data Result = You | Me deriving (Eq,Ord,Show)

-- 1. Throw two dice. I win if the difference is 0, 1, 2. You win if it is 3, 4, 5.

dist1 :: Dist Result
dist1 = do
  diff <- abs (die - die)
  return $ if diff `elem` [1,2,3]
    then Me
    else You

-- 2. Throw two dice. I win if 2 or 5 shows on either die, otherwise you win.

dist2 :: Dist Result
dist2 = do
  a <- die
  b <- die
  return $ if a `elem` [2,5] || b `elem` [2,5]
    then Me
    else You