module Balls where

{-
Imagine an urn with 10 balls, 5 of which are white and 5 are black. You draw
three balls at random. What is the probability that they are all black?
-}

--import qualified Data.List as List
--import qualified Data.Set  as Set
import Control.Probability

data Color = White | Black deriving (Eq,Ord,Show)



dist :: Dist Bool
dist = all (== Black) <$> sampleWithoutReplacement 3 bag
  where
    bag = replicate 5 White ++ replicate 5 Black