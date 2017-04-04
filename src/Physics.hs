module Physics where

import Geometry

type Distance = Double
type Acceleration = Vector
type Velocity = Vector

gravity = 70.0

accelerationValue :: Distance -> Double
accelerationValue r = gravity / (max 0.05 (r * r))

changeVelocity :: Velocity -> Acceleration -> Velocity
changeVelocity v a = v +++ a