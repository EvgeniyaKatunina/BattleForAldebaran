module GameParameters where

-- settings

defaultNShips :: Int
defaultNShips = 3

-- physics

gravity :: Double
gravity = 600.0

-- control

rotationSpeed :: Double
rotationSpeed = 0.1

acceleration :: Double
acceleration = 0.04

spawnOrbitalSpeed :: Double
spawnOrbitalSpeed = 1.4

-- sizes & forms

width :: Double
width = 600

height :: Double
height = 600

planetRadius :: Double
planetRadius = 80.0

triangleSpread :: Double
triangleSpread = pi / 4

triangleRadius :: Double
triangleRadius = 8.0

debrisPiecesNumber :: (Int, Int)
debrisPiecesNumber = (5, 8)

debrisRadius :: (Double, Double)
debrisRadius = (2.0, triangleRadius / 2.5)

debrisLifetime :: (Int, Int)
debrisLifetime = (100, 200)

debrisShift :: (Double, Double)
debrisShift = (0, triangleRadius)

explosionTime :: Int
explosionTime = 7

explosionParticles :: Int
explosionParticles = 6

explosionShift :: (Double, Double)
explosionShift = (0, triangleRadius)

explosionRadius :: (Double, Double)
explosionRadius = (triangleRadius / 3, triangleRadius / 2)

-- time & speed

frameTime :: Int
frameTime = 20

shotCooldownConst :: Int
shotCooldownConst = 15

shotSpeed :: Double
shotSpeed = 3.0

shotLifetime :: Int
shotLifetime = 250

shotSafeTime :: Int
shotSafeTime = 30

debrisSpeedSpread :: (Double, Double)
debrisSpeedSpread = (-pi/8, pi/8)

explosionSpeed :: (Double, Double)
explosionSpeed = (1.5, 2.5)
