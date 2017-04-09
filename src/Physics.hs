module Physics where

import Geometry

type Distance = Double
type Acceleration = Vector
type Velocity = Vector

gravity = 600.0

accelerationValue :: Distance -> Double
accelerationValue r = gravity / max 10.0 (r * r)

changeVelocity :: Velocity -> Acceleration -> Velocity
changeVelocity v a = v +++ a