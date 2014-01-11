module Triangle where

import Control.Probability
import Control.Probability.MonteCarlo

{-

A triangle is chosen at random. What is the probability that it is obtuse?

By a "random triangle" we consider the case of three points chosen at random
on the unit circle.

-}

type Point = (Double,Double)
type Angle =  Double
type Triangle = (Point,Point,Point)

piBy2 = pi / 2 :: Angle

point = do
    angle <- randomR (0, 2*pi)
    returning (cos angle, sin angle)

triangle = liftP3 (,,) point point point

distance (x,y) (x',y') = sqrt $ (x-x')^2 + (y-y')^2

-- using a^2 + b^2 - 2ab cos C = c^2; solve for C
angle p q r = let a = distance p q
                  b = distance q r
                  c = distance r p
               in acos $ (a^2 + b^2 - c^2) / (2 * a * b)

obtuseAngle angle = angle > piBy2

isTriangleObtuse (p,q,r) = let a = angle p q r
                               b = angle q r p
                               c = angle r p q
                            in any obtuseAngle [a,b,c]

dist :: MC Double Bool
dist = isTriangleObtuse ?? triangle

runMonteCarlo n = do
  putStrLn "Probability of getting an obtuse triangle:"
  sampleMC n dist >>= putStrLn . prettyPrintGeneric