module Alarm where

import Control.Probability

bool :: (Probability p, MonadProb p m) => p -> m p Bool
bool p = choose p True False

filterDist :: (Ord a, MonadProb p m) => (a -> Bool) -> m p a -> m p a
filterDist f m = do x <- m 
                    condition (f x)
                    returning x

(>>=?) :: (Ord a, MonadProb p m) => m p a -> (a -> Bool) -> m p a
(>>=?) = flip filterDist

(?=<<) :: (Ord a, MonadProb p m) => (a -> Bool) -> m p a -> m p a
(?=<<) = filterDist


-- | prior burglary is 1%
b :: Dist Bool
b = bool 0.01

-- | prior earthqauke is 0.1%
e :: Dist Bool
e = bool 0.001

-- | conditional prob of alarm | burglary, earthquake
a :: Bool -> Bool -> Dist Bool
a b0 e0 =
    case (b0,e0) of
        (False,False) -> bool 0.01
        (False,True)  -> bool 0.1
        (True,False)  -> bool 0.7
        (True,True)   -> bool 0.9

-- | conditional prob of john calling | alarm
j :: Bool -> Dist Bool
j a = if a then bool 0.8 else bool 0.05

-- | conditional prob of mary calling | alarm
m :: Bool -> Dist Bool
m a = if a then bool 0.9 else bool 0.1


-- | full joint distribution
data Burglary = B
    { burglary   :: Bool
    , earthquake :: Bool
    , alarm      :: Bool
    , john       :: Bool
    , mary       :: Bool }
    deriving (Eq,Ord,Show)

joint :: Dist Burglary
joint = do
    b' <- b
    e' <- e
    a' <- a b' e'
    j' <- j a'
    m' <- m a'
    returning $ B b' e' a' j' m'

-- | probability that mary calls given john calls
mj = mary ?? john ?=<< joint

-- | probability of burglary given mary calls
bm = burglary ?? mary ?=<< joint

-- | probability of burglary given john calls
bj = burglary ?? john ?=<< joint