module Cards where

import           Data.Map (Map, (!))
import qualified Data.Map as Deck     -- this is weird, but you'll understand...
import           Control.Probability

{-----------------------------------------------------------

We deal cards in succession from a deck of 2N cards,
featuring N red and N black. If they are the same color,
player 1 gets a point. If they are a different color, player
2 gets a point. Who is most likely to win?

-----------------------------------------------------------}


data Color = Red | Black deriving (Eq,Ord,Show)

newDeck n = Deck.fromList [(Red, n), (Black, n)]

finished deck = (deck ! Red == 0) && (deck ! Black == 0)

draw1 deck = do
  let reds   = fromIntegral (deck ! Red)
      blacks = fromIntegral (deck ! Black)
      p      = reds / (reds + blacks)
  color <- choose p Red Black
  return (color, Deck.update (Just . subtract 1) color deck)

data Score = Score { p1 :: !Int, p2 :: !Int } deriving (Eq,Ord,Show)

game :: Int -> Dist Score
game n = play (Score 0 0) (newDeck n)
 where
  play score@(Score p1 p2) deck =
    if finished deck
      then return score
      else do
        (a,deck')  <- draw1 deck
        (b,deck'') <- draw1 deck'
        if a == b
          then play (Score (p1+1) p2) deck''
          else play (Score p1 (p2+1)) deck''
