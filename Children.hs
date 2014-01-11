module Children where

import Control.Probability

data Child = Boy | Girl deriving (Eq,Ord,Show)

numBoys :: Dist Int
numBoys = do
  a <- child
  b <- child
  let numberOfBoys = countBoys [a,b]
  condition (numberOfBoys > 0)
  returning (numberOfBoys)
 where
  child     = choose 0.5 Boy Girl
  countBoys = length . filter (== Boy)

main = do
  putStrLn "Probability distribution for number of boys:"
  printProb numBoys
