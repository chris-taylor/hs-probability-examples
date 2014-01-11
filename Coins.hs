module Coins where

import Control.Probability

data Coin = Head | Tail deriving (Eq,Ord,Show)

dist :: Dist Coin
dist = do
  coin <- choose' (999/1000) fair biased -- use choose' because 'fair' and 'biased' are not ordered
  tosses <- replicateM 10 coin
  condition (tosses == replicate 10 Head)
  nextToss <- coin
  return nextToss
 where
  fair   = choose (1/2) Head Tail
  biased = certainly Head