module Tennis where

import Control.Probability

{-

As a condition for the acceptance to a tennis club a novice player N is
set to meet two members of the club, G (good) and T (top) in three games. 
In order to be accepted, N must win against both G and T in two successive
games. N must choose one of the two schedules: playing G, T, G or T, G, T.
Which one should he choose?

-}

data Result = Win | Lose deriving (Eq,Ord,Show)

good, top :: Dist Result
good = choose (5/10) Win Lose
top  = choose (2/10) Win Lose

play p1 p2 p3 = do
    r1 <- p1
    r2 <- p2
    if r2 == Lose
        then certainly Lose
        else if r1 == Win
            then certainly Win
            else do
                r3 <- p3
                if r3 == Win
                    then certainly Win
                    else certainly Lose

playGTG = play good top good
playTGT = play top good top