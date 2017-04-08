module SpaceGame where

import Graphics.UI.Fungen
import Graphics.UI.Fungen.Objects
import Graphics.Rendering.OpenGL (GLdouble)
import Geometry
import Control.Monad
import Physics

data GameAttribute = Score Int

width = 400
height = 400
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble

nShips = 3

planetManagerName = "planetGroup"
spaceshipManagerNames = ["ships1", "ships2"]

playerColors = [Color 0.0 1.0 0.0,
                Color 0.0 0.0 1.0]

main :: IO ()
main = do
  let winConfig = ((100,80),(width,height),"Space game")
      gameMap = colorMap 0 0 0 w h
      planet = objectGroup planetManagerName [createPlanet]
      spaceships1 = objectGroup (spaceshipManagerNames !! 0) $
                        map (\i -> createSpaceship 0 (Color 0.0 1.0 0.0) (w/2 + 50 + 5 * i, h/2))
                            [1..nShips]
      spaceships2 = objectGroup (spaceshipManagerNames !! 1) $
                        map (\i -> createSpaceship 1 (Color 0.0 0.0 1.0) (w/2 - 50 - 5 * i, h/2 + 30))
                            [1..nShips]
  funInit winConfig gameMap [planet, spaceships1, spaceships2] () (()) [] (gameCycle) (Timer 30) []

createPlanet :: GameObject ObjectAttributes
createPlanet =
  let planetPic = Basic (Circle 20.0 1.0 0.0 0.0 Filled)
  in object "planet" planetPic False (w/2,h/2) (0,0) NoAttributes

data Color = Color { red :: Float, green :: Float, blue :: Float }
type Name = String

data ObjectAttributes = NoAttributes |
                        ShipAttributes { angle :: Double, ownerId :: Int }

createSpaceship :: Int -> Color -> Point -> GameObject ObjectAttributes
createSpaceship ownerId color pos =
  let Color r g b = color in
  let spaceshipPic = Basic (Circle 2.0 r g b Filled)
  in object "" spaceshipPic False pos (0, 1.0) (ShipAttributes 0.0 ownerId)

_triangleSpread = pi / 8
_triangleRadius = 5

shipTriangle :: Vector -> Double -> [Point2D]
shipTriangle (vx, vy) angle =
    let vangle = atan (vy / vx) + (if signum vx == -1 then pi else 0)
        noseAngle = vangle + angle
        angles = [noseAngle, noseAngle + pi - _triangleSpread, noseAngle + pi + _triangleSpread]
    in map (\a -> (((cos a) * _triangleRadius), (sin a) * _triangleRadius)) angles

shipPoly :: Vector -> Double -> Int -> ObjectPicture
shipPoly speed angle ownerId =
    let Color r g b = playerColors !! ownerId in
    Basic $ Polyg (shipTriangle speed angle) r g b Filled

updateShipPictures :: IOGame () ObjectAttributes () () ()
updateShipPictures = do
    managers <- getObjectManagers
    newManagers <- forM spaceshipManagerNames (\managerName -> do
        let manager = searchObjectManager managerName managers
        let objects = getObjectManagerObjects manager
        newObjects <- forM objects (\o -> do
            name <- getObjectName o
            position <- getObjectPosition o
            speed <- getObjectSpeed o
            a@(ShipAttributes angle ownerId) <- getObjectAttribute o
            return $ object name (shipPoly speed angle ownerId) False position speed a
         )
        return $ objectGroup managerName newObjects
     )
    planetGroup <- findObjectManager "planetGroup"
    setObjectManagers (planetGroup:newManagers)

performGravity :: IOGame () ObjectAttributes () () ()
performGravity = forM_ spaceshipManagerNames (\managerName -> do
        manager <- findObjectManager managerName
        let ships = getObjectManagerObjects manager
        forM_ ships handleGravity
    )

handleGravity :: (GameObject ObjectAttributes) -> IOGame () ObjectAttributes () () ()
handleGravity ship = do
    planet <- findObject "planet" "planetGroup"
    (px, py) <- getObjectPosition planet

    (vx, vy) <- getObjectSpeed ship
    (sx, sy) <- getObjectPosition ship
    let (dx, dy) = diff (px, py) (sx, sy)
    let r = distance (px, py) (sx, sy)
    let acceleration = (accelerationValue r) *** (ort (px, py) (sx, sy))
    setObjectSpeed ((vx , vy) +++ acceleration) ship

gameCycle :: IOGame () ObjectAttributes () () ()
gameCycle = do
  performGravity
  updateShipPictures