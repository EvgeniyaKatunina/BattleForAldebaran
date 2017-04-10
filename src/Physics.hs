module Physics where

import Geometry
import GameParameters

type Distance = Double
type Acceleration = Vector
type Velocity = Vector

accelerationValue :: Distance -> Double
accelerationValue r = gravity / max 10.0 (r * r)

changeVelocity :: Velocity -> Acceleration -> Velocity
changeVelocity v a = v ^+^ a

addImpulse :: Double -> Double -> Vector -> Vector -> Vector
addImpulse m1 m2 v1 v2 = let ms = m1 + m2 in
                         ((m1 / ms) #*^ v1) ^+^ ((m2 / ms) #*^ v2)