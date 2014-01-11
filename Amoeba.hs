module Amoeba where

import Control.Probability
import Control.Probability.MonteCarlo

{-

A population starts with a single amoeba. For this one and for the generations
thereafter, there is a probability of 3/4 that an individual amoeba will split to
create two amoebas, and a 1/4 probability that it will die out without producing
offspring. What is the probability that the family tree of the original amoeba
will go on forever?

Again, we approximate with a finite number of generations. As the number of
generations goes to infinit, the answer becomes more accurate.

-}

data Result = Extinct | Survived deriving (Eq,Ord,Show)

type Population = Int

breed :: Population -> Dist Population
breed n = sum (replicate n amoeba)
 where
  amoeba = choose 0.75 2 0

--breed :: (MonadProb p m) => Population -> m p Population
--breed 0 = certainly 0
--breed n = do
--  a <- numOffSpring
--  b <- breed (n-1)
--  certainly (a + b)
-- where
--  numOffSpring = choose 0.75 2 0

survival :: Int -> Dist Result
survival maxGenerations = go maxGenerations 1
 where
  go 0 pop = certainly Survived
  go _ 0   = certainly Extinct
  go n pop = breed pop >>= go (n-1)

