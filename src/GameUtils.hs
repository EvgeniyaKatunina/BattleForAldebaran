{-# LANGUAGE TemplateHaskell #-}

module GameUtils where

import Control.Lens
import GameTypes
import Geometry
import GameParameters
import Control.Monad
import Control.Monad.Loops
import Graphics.UI.Fungen.Objects
import Graphics.Rendering.OpenGL (GLdouble)
import Graphics.UI.Fungen hiding (when)

gameMap :: GameMap ()
gameMap = colorMap 0 0 0 (fromIntegral $ round width) (fromIntegral $ round height)

playerColors :: [Color]
playerColors = [Color 0.0 1.0 0.0, Color 0.3 0.3 1.0]

shotColors :: [Color]
shotColors = [Color 1.0 1.0 0.0, Color 1.0 0.0 1.0]

spaceshipManagerNames :: [String]
spaceshipManagerNames = ["ships1", "ships2"]

bulletsManagerName :: String
bulletsManagerName = "bullets"

explosionsManagerName :: String
explosionsManagerName = "explosions"

planetManagerName :: String
planetManagerName = "planetGroup"

planet :: GameObject ObjectAttributes
planet = let planetPic = Basic (Circle planetRadius 1.0 0.0 0.0 Filled)
         in object "planet" planetPic False (width / 2, height / 2) (0,0) NoAttributes

debrisManagerName :: String
debrisManagerName = "debrisGroup"

createSpaceship :: Int -> Int -> Color -> Point -> GameObject ObjectAttributes
createSpaceship ownerId shipId color pos =
  let Color r g b = color
      speed = (0, case ownerId of 0 -> spawnOrbitalSpeed; 1 -> -spawnOrbitalSpeed)
      angle = 0.0
      spaceshipPic = shipPoly speed angle ownerId triangleRadius False
  in object ("ship" ++ show ownerId ++ show shipId) spaceshipPic False pos speed (ShipAttributes angle ownerId shotCooldownConst)

shipTriangle :: Vector -> Double -> Double -> [Point2D]
shipTriangle (vx, vy) angle size =
  let nose = noseAngle (vx, vy) angle
      angles = [nose, nose + pi - triangleSpread, nose + pi + triangleSpread]
  in map (\a -> (cos a * size, sin a * size)) angles

shipPoly :: Vector -> Double -> Int -> Double -> Bool -> ObjectPicture
shipPoly speed angle ownerId size isSelected =
  let Color r g b = playerColors !! ownerId in
  Basic $ Polyg (shipTriangle speed angle size) r g b (if isSelected then Filled else Unfilled)

initialAttrs :: GameAttribute
initialAttrs = GameAttribute BeforeStart (map (\_ -> Player 0) [0, 1]) 0 defaultNShips

createGroups :: GameAttribute -> [ObjectManager ObjectAttributes]
createGroups attr =
    let planetGroup = objectGroup planetManagerName [planet] in
    case attr^.phase of
        BeforeStart -> [planetGroup]
        Running -> [planetGroup, bulletsGroup, debrisGroup, explosionsGroup, spaceships1, spaceships2] where
            n = attr^.nShips
            spaceships1 = objectGroup (head spaceshipManagerNames) $
                          map (\i -> createSpaceship 0 i (Color 0.0 1.0 0.0) (width / 2 + width / 3 + 10 * fromIntegral i, height / 2))
                          [1..n]
            spaceships2 = objectGroup (spaceshipManagerNames !! 1) $
                          map (\i -> createSpaceship 1 i (Color 0.0 0.0 1.0) (width / 2 - width / 3 - 10 * fromIntegral i, height /2 + 30))
                          [1..n]
            debrisGroup = objectGroup debrisManagerName []
            explosionsGroup = objectGroup explosionsManagerName []
            bulletsGroup = objectGroup bulletsManagerName []
        AfterEnd _ -> [planetGroup]

playerHasShips :: Int -> SGame Bool
playerHasShips playerId = do
    manager <- findObjectManager (spaceshipManagerNames !! playerId)
    return (not $ null $ getObjectManagerObjects manager)

playerHasShipsInBound :: Int -> SGame Bool
playerHasShipsInBound playerId = do
    manager <- findObjectManager (spaceshipManagerNames !! playerId)
    let c o = do
                p <- getObjectPosition o
                return $ isInBound p
    shipsInBound <- filterM c $ getObjectManagerObjects manager
    return (not $ null shipsInBound)

whenRunningGame :: SGame () -> SGame ()
whenRunningGame m = getGameAttribute >>= \a -> when (a^.phase == Running) m

whenPlayerHasShips :: Int -> SGame () -> SGame ()
whenPlayerHasShips playerId m = playerHasShips playerId >>= \b -> when b m

rotateShip :: Double -> GameObject ObjectAttributes -> SGame ()
rotateShip angleDiff ship = do
    attr <- getObjectAttribute ship
    setObjectAttribute (angle %~ (+angleDiff) $ attr) ship

accelerateShip :: Double -> GameObject ObjectAttributes -> SGame()
accelerateShip speedDiff ship = do
    ShipAttributes angle ownerId _ <- getObjectAttribute ship
    (vx, vy) <- getObjectSpeed ship
    let nangle = noseAngle (vx, vy) angle
    let (dvx, dvy) = (cos nangle * speedDiff, sin nangle * speedDiff)
    setObjectSpeed (vx + dvx, vy + dvy) ship

nextBulletId :: SGame Int
nextBulletId = do
    attr <- getGameAttribute
    setGameAttribute $ bulletId %~ (+1) $ attr
    return $ attr^.bulletId

replaceObjectGroup :: String -> ObjectManager t -> [ObjectManager t] -> [ObjectManager t]
replaceObjectGroup name m [] = []
replaceObjectGroup name m (x:xs) = if name == getObjectManagerName x then m:xxs else x:xxs where
                                        xxs = replaceObjectGroup name m xs

setCurrentShipIndex :: Int -> Int -> SGame ()
setCurrentShipIndex playerId shipIndex = do
    attr <- getGameAttribute
    let player = (attr^.players) !! playerId
    setGameAttribute $ attr { _players = _players attr & element playerId .~ (player { _currentShip = shipIndex }) }

getCurrentShipIndex :: Int -> SGame Int
getCurrentShipIndex playerId = do
    attr <- getGameAttribute
    manager <- findObjectManager (spaceshipManagerNames !! playerId)
    let ships = getObjectManagerObjects manager
    let ship = attr ^. singular (players . ix playerId . currentShip)
    return $ ship `mod` length ships

getCurrentShip :: Int -> SGame (GameObject ObjectAttributes)
getCurrentShip playerId = do
    shipId <- getCurrentShipIndex playerId
    manager <- findObjectManager (spaceshipManagerNames !! playerId)
    let ships = getObjectManagerObjects manager
    return $ ships !! shipId

isInBound :: Point -> Bool
isInBound (px, py) = px >= -triangleRadius && px <= width + triangleRadius &&
                     py >= -triangleRadius && py <= height + triangleRadius

switchToNextShip :: Int -> Int -> SGame ()
switchToNextShip d playerId = do
    canSwitch <- playerHasShipsInBound playerId
    when canSwitch $ untilM_ next inBound where
        next = do
            currentShipId <- getCurrentShipIndex playerId
            attr <- getGameAttribute
            let player = (attr^.players) !! playerId
            setGameAttribute $ (players . ix playerId . currentShip) %~ (+ d) $ attr
        inBound = do
            currentShipId <- getCurrentShipIndex playerId
            manager <- findObjectManager $ spaceshipManagerNames !! playerId
            p <- getObjectPosition $ getObjectManagerObjects manager !! currentShipId
            return $ isInBound p

endGame :: [Int] -> SGame ()
endGame winnerIds = do
    attr <- getGameAttribute
    let a = phase .~ AfterEnd winnerIds $ attr
    setObjectManagers $ createGroups a
    setGameAttribute a

printLines :: [String] -> BitmapFont -> Double -> Point -> Color -> SGame ()
printLines xs font lineHeight (px, py) (Color r g b) =
    forM_ (zip (if lineHeight > 0 then xs else reverse xs) [0..]) $ \(l, n) ->
        printOnScreen l font (px, py - lineHeight * n) r g b