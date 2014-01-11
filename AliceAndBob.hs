{-# LANGUAGE FlexibleContexts #-}

module AliceAndBob where

{-

Alice and Bob play a fair game repeatedly for one nickel each game. If originally
Alice has n nickels and Bob has m nickels, what is the chance of Alice winning
all of Bob's money, if the game goes on until one person has lost all of their
money.

In this problem the probability tree is infinite. We approximate it with a
finite tree.

-}

import Control.Probability
import Control.Probability.MonteCarlo

data Result = Alice | Bob | Draw deriving (Eq,Ord,Show)

dist :: MonadProb Double m => Int -> Int -> Int -> m Double Result
dist nGames alice bob = go nGames alice bob
    where
        go 0 _ _ = certainly Draw   -- max number of games reached
        go _ 0 _ = certainly Bob    -- alice out of coins
        go _ _ 0 = certainly Alice  -- bob out of coins
        go n a b = do
            result <- playGame
            if result == Alice
                then go (n-1) (a+1) (b-1)
                else go (n-1) (a-1) (b+1)

        playGame = choose 0.5 Alice Bob


runExact  n a b = printProb (dist n a b)

runApprox n a b = sampleMC 10000 (dist n a b) >>= putStrLn . prettyPrintGeneric