module Main where

import Graphics.UI.Fungen
import Graphics.UI.Fungen.Objects
import Graphics.Rendering.OpenGL (GLdouble)
import Geometry
import Control.Monad
import Physics

data Player = Player { currentShip :: Int }
data GameAttribute = GameAttribute { players :: [Player] }

width = 600
height = 600
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble

nShips = 3

planetManagerName = "planetGroup"
spaceshipManagerNames = ["ships1", "ships2"]

playerColors = [Color 0.0 1.0 0.0,
                Color 0.0 0.0 1.0]

playerKeys = [[SpecialKey KeyRight, SpecialKey KeyLeft, SpecialKey KeyUp, SpecialKey KeyCtrlR],
              [Char 'd', Char 'a', Char 'w', Char 'q']]

replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

main :: IO ()
main = do
  let winConfig = ((100,80),(width,height),"Space game")
      gameMap = colorMap 0 0 0 w h
      planet = objectGroup planetManagerName [createPlanet]
      spaceships1 = objectGroup (spaceshipManagerNames !! 0) $
                        map (\i -> createSpaceship 0 (Color 0.0 1.0 0.0) (w/2 + w/4 + 10 * i, h/2))
                            [1..nShips]
      spaceships2 = objectGroup (spaceshipManagerNames !! 1) $
                        map (\i -> createSpaceship 1 (Color 0.0 0.0 1.0) (w/2 - w/4 - 10 * i, h/2 + 30))
                            [1..nShips]
      input = concatMap createInput (zip [0..] playerKeys) where
              createInput (player, [r, l, u, n]) = [
                (r, StillDown, rotateCurrentShip player (-0.1)),
                (l, StillDown, rotateCurrentShip player 0.1),
                (u, StillDown, accelerateCurrentShip player 0.02),
                (n, Press, switchToNextShip player)
               ]
      initialAttrs = GameAttribute $ map (\i -> Player 0) [0, 1]
  funInit winConfig gameMap [planet, spaceships1, spaceships2] () initialAttrs input (gameCycle) (Timer 30) []

createPlanet :: GameObject ObjectAttributes
createPlanet =
  let planetPic = Basic (Circle 60.0 1.0 0.0 0.0 Filled)
  in object "planet" planetPic False (w/2,h/2) (0,0) NoAttributes

data Color = Color { red :: Float, green :: Float, blue :: Float }
type Name = String

data ObjectAttributes = NoAttributes |
                        ShipAttributes { angle :: Double, ownerId :: Int }

createSpaceship :: Int -> Color -> Point -> GameObject ObjectAttributes
createSpaceship ownerId color pos =
  let Color r g b = color in
  let spaceshipPic = Basic (Circle 2.0 r g b Filled)
  in object "" spaceshipPic False pos (0, case ownerId of 0 -> 1.2; 1 -> -1.2) (ShipAttributes 0.0 ownerId)

_triangleSpread = pi / 7
_triangleRadius = 8

shipTriangle :: Vector -> Double -> [Point2D]
shipTriangle (vx, vy) angle =
    let vangle = atan (vy / vx) + (if signum vx == -1 then pi else 0)
        noseAngle = vangle + angle
        angles = [noseAngle, noseAngle + pi - _triangleSpread, noseAngle + pi + _triangleSpread]
    in map (\a -> (((cos a) * _triangleRadius), (sin a) * _triangleRadius)) angles

shipPoly :: Vector -> Double -> Int -> Bool -> ObjectPicture
shipPoly speed angle ownerId isSelected =
    let Color r g b = playerColors !! ownerId in
    Basic $ Polyg (shipTriangle speed angle) r g b (if isSelected then Filled else Unfilled)

updateShipPictures :: IOGame GameAttribute ObjectAttributes () () ()
updateShipPictures = do
    managers <- getObjectManagers
    newManagers <- forM (zip [0..] spaceshipManagerNames) (\(playerId, managerName) -> do
        let manager = searchObjectManager managerName managers
        let objects = getObjectManagerObjects manager
        currentShipIndex <- getCurrentShipIndex playerId
        newObjects <- forM (zip [0..] objects) (\(shipId, o) -> do
            name <- getObjectName o
            position <- getObjectPosition o
            speed <- getObjectSpeed o
            a@(ShipAttributes angle ownerId) <- getObjectAttribute o
            return $ object name (shipPoly speed angle ownerId (shipId == currentShipIndex)) False position speed a
         )
        return $ objectGroup managerName newObjects
     )
    planetGroup <- findObjectManager "planetGroup"
    setObjectManagers (planetGroup:newManagers)

performGravity :: IOGame GameAttribute ObjectAttributes () () ()
performGravity = forM_ spaceshipManagerNames (\managerName -> do
        manager <- findObjectManager managerName
        let ships = getObjectManagerObjects manager
        forM_ ships handleGravity
    )

getCurrentShipIndex :: Int -> IOGame GameAttribute ObjectAttributes () () Int
getCurrentShipIndex playerId = do
    attr <- getGameAttribute
    manager <- findObjectManager (spaceshipManagerNames !! playerId)
    let ships = (getObjectManagerObjects manager)
    return $ currentShip (players attr !! playerId) `mod` (length ships)

getCurrentShip :: Int -> IOGame GameAttribute ObjectAttributes () () (GameObject ObjectAttributes)
getCurrentShip playerId = do
    shipId <- getCurrentShipIndex playerId
    manager <- findObjectManager (spaceshipManagerNames !! playerId)
    let ships = (getObjectManagerObjects manager)
    return $ ships !! shipId

rotateCurrentShip :: Int -> Double -> Modifiers -> Position -> IOGame GameAttribute ObjectAttributes () () ()
rotateCurrentShip playerId angleDiff modifiers position =
    getCurrentShip playerId >>= rotateShip angleDiff

rotateShip :: Double -> (GameObject ObjectAttributes) -> IOGame GameAttribute ObjectAttributes () () ()
rotateShip angleDiff ship = do
    attr <- getObjectAttribute ship
    setObjectAttribute (attr { angle = (angle attr) + angleDiff }) ship

accelerateCurrentShip :: Int -> Double -> Modifiers -> Position -> IOGame GameAttribute ObjectAttributes () () ()
accelerateCurrentShip playerId speedDiff modifiers position = do
    ship <- getCurrentShip playerId
    ShipAttributes angle ownerId <- getObjectAttribute ship
    (vx, vy) <- getObjectSpeed ship
    let noseAngle = angle + atan (vy / vx) + (if signum vx == -1 then pi else 0) --todo generify angle calculation
    let (dvx, dvy) = (cos noseAngle * speedDiff, sin noseAngle * speedDiff)
    setObjectSpeed (vx + dvx, vy + dvy) ship

switchToNextShip :: Int -> Modifiers -> Position -> IOGame GameAttribute ObjectAttributes () () ()
switchToNextShip playerId _ _ = do
    currentShipId <- getCurrentShipIndex playerId
    let newShipId = currentShipId + 1
    attr <- getGameAttribute
    let player = (players attr) !! playerId
    setGameAttribute $ attr { players = replaceAtIndex playerId (player { currentShip = newShipId }) (players attr) }

handleGravity :: (GameObject ObjectAttributes) -> IOGame GameAttribute ObjectAttributes () () ()
handleGravity ship = do
    planet <- findObject "planet" "planetGroup"
    (px, py) <- getObjectPosition planet

    (vx, vy) <- getObjectSpeed ship
    (sx, sy) <- getObjectPosition ship
    let (dx, dy) = diff (px, py) (sx, sy)
    let r = distance (px, py) (sx, sy)
    let acceleration = (accelerationValue r) *** (ort (px, py) (sx, sy))
    setObjectSpeed ((vx , vy) +++ acceleration) ship

gameCycle :: IOGame GameAttribute ObjectAttributes () () ()
gameCycle = do
  performGravity
  updateShipPictures