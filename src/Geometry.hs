module Geometry where

type Point = (Double, Double)
type Vector = (Double, Double)

getX :: (a, a) -> a
getX = fst

getY :: (a, a) -> a
getY = snd

distance :: Point -> Point -> Double
distance pa pb = sqrt (dx * dx + dy * dy)
    where d = diff pa pb
          dx = getX d
          dy = getY d

diff :: Point -> Point -> Vector
diff pa pb = (getX pa - getX pb, getY pa - getY pb)

ort :: Point -> Point -> Vector
ort pa pb = (dx / r, dy / r)
    where d = diff pa pb
          dx = getX d
          dy = getY d
          r = distance pa pb

(***) :: Double -> Vector -> Vector
koeff *** vector = (getX vector * koeff, getY vector * koeff)

(+++) :: Vector -> Vector -> Vector
vectorA +++ vectorB = (getX vectorA + getX vectorB, getY vectorA + getY vectorB)

