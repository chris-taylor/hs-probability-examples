module DiceGame where

import Control.Probability

die :: Dist Double
die = uniform [1..6]

game 0 = certainly 0
game n = do
  x <- die
  if x > expectation next then return x else next
 where
  next = game (n-1)