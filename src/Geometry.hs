module Geometry where

newtype Point = Point (Double, Double)
newtype Vector = Vector (Double, Double)

distance :: Point -> Point -> Double
distance pa pb = sqrt (dx * dx + dy * dy)
    where Vector (dx, dy) = diff pa pb

ort :: Point -> Point -> Vector
ort pa pb = Vector (dx / r, dy / r)
    where Vector (dx, dy) = diff pa pb
          r = distance pa pb

diff :: Point -> Point -> Vector
diff (Point (ax, ay)) (Point (bx, by)) = Vector (ax - bx, ay - by)

(#*^) :: Double -> Vector -> Vector
coeff #*^ Vector (vx, vy) = Vector (vx * coeff, vy * coeff)

(^+^) :: Vector -> Vector -> Vector
Vector (v1x, v1y) ^+^ Vector (v2x, v2y) = Vector (v1x + v2x, v1y + v2y)

(.+^) :: Point -> Vector -> Point
Point (px, py) .+^ Vector (vx, vy) = Point (px + vx, py + vy)

noseAngle :: Vector -> Double -> Double
noseAngle v angle = directionAngle v + angle

directionAngle :: Vector -> Double
directionAngle (Vector (vx, vy)) = atan (vy / vx) + (if signum vx == -1 then pi else 0)