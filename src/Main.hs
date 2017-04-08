module Main where

import Graphics.UI.Fungen hiding (when)
import Graphics.UI.Fungen.Objects
import Graphics.Rendering.OpenGL (GLdouble)
import Geometry
import Control.Monad
import Physics
import Control.Lens

data Color = Color { red :: Float, green :: Float, blue :: Float }

data Player = Player { currentShip :: Int }
data GameAttribute = GameAttribute { players :: [Player], bulletId :: Int }

data ObjectAttributes = NoAttributes |
                        ShipAttributes { angle :: Double, ownerId :: Int, shotCooldown :: Int } |
                        BulletAttributes { lifetime :: Int, fromShipName :: [Char] }

type SGame a = IOGame GameAttribute ObjectAttributes () () a

nShips = 3

playerColors = [Color 0.0 1.0 0.0,
                Color 0.3 0.3 1.0]
shotColors = [Color 1.0 1.0 0.0,
              Color 1.0 0.0 1.0]
playerKeys = [[SpecialKey KeyRight, SpecialKey KeyLeft, SpecialKey KeyUp, Char '.', SpecialKey KeyDown],
              [Char 'd', Char 'a', Char 'w', Char 'q', Char 's']]

spaceshipManagerNames = ["ships1", "ships2"]

bulletsManagerName = "bullets"
bulletsGroup = objectGroup bulletsManagerName []

planetRadius = 80.0
planetManagerName = "planetGroup"

planet :: GameObject ObjectAttributes
planet =
  let planetPic = Basic (Circle planetRadius 1.0 0.0 0.0 Filled)
  in object "planet" planetPic False (w/2,h/2) (0,0) NoAttributes

planetGroup = objectGroup planetManagerName [planet]

width = 600
height = 600
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble

main :: IO ()
main = do
  let winConfig = ((100,80),(width,height),"Space game")
      gameMap = colorMap 0 0 0 w h
      spaceships1 = objectGroup (spaceshipManagerNames !! 0) $
                        map (\i -> createSpaceship 0 i (Color 0.0 1.0 0.0) (w/2 + w/3 + 10 * (fromIntegral i), h/2))
                            [1..nShips]
      spaceships2 = objectGroup (spaceshipManagerNames !! 1) $
                        map (\i -> createSpaceship 1 i (Color 0.0 0.0 1.0) (w/2 - w/3 - 10 * (fromIntegral i), h/2 + 30))
                            [1..nShips]
      input = concatMap createInput (zip [0..] playerKeys) where
              createInput (player, [r, l, u, n, s]) = [
                (r, StillDown, rotateCurrentShip player (-0.1)),
                (l, StillDown, rotateCurrentShip player 0.1),
                (u, StillDown, accelerateCurrentShip player 0.04),
                (n, Press, switchToNextShip player),
                (s, Press, shoot player)
               ]
      initialAttrs = GameAttribute (map (\i -> Player 0) [0, 1]) 0
  funInit winConfig gameMap [planetGroup, bulletsGroup, spaceships1, spaceships2] () initialAttrs input (gameCycle) (Timer 30) []

_shotCooldown = 15

createSpaceship :: Int -> Int -> Color -> Point -> GameObject ObjectAttributes
createSpaceship ownerId shipId color pos =
  let Color r g b = color
      speed = (0, case ownerId of 0 -> 1.4; 1 -> -1.4)
      angle = 0.0
      spaceshipPic = shipPoly speed angle ownerId False
  in object ("ship" ++ (show ownerId) ++ (show shipId)) spaceshipPic False pos speed (ShipAttributes angle ownerId _shotCooldown)

_triangleSpread = pi / 4
_triangleRadius = 8

noseAngle :: Vector -> Double -> Double
noseAngle (vx, vy) angle =
    let vangle = atan (vy / vx) + (if signum vx == -1 then pi else 0)
    in vangle + angle

shipTriangle :: Vector -> Double -> [Point2D]
shipTriangle (vx, vy) angle =
    let nose = noseAngle (vx, vy) angle
        angles = [nose, nose + pi - _triangleSpread, nose + pi + _triangleSpread]
    in map (\a -> (((cos a) * _triangleRadius), (sin a) * _triangleRadius)) angles

shipPoly :: Vector -> Double -> Int -> Bool -> ObjectPicture
shipPoly speed angle ownerId isSelected =
    let Color r g b = playerColors !! ownerId in
    Basic $ Polyg (shipTriangle speed angle) r g b (if isSelected then Filled else Unfilled)

updateShipPictures :: SGame ()
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
            a@(ShipAttributes angle ownerId _) <- getObjectAttribute o
            return $ object name (shipPoly speed angle ownerId (shipId == currentShipIndex)) False position speed a
         )
        return $ objectGroup managerName newObjects
     )
    setObjectManagers $ foldr (\a b -> replaceObjectGroup (getObjectManagerName a) a b) managers newManagers

replaceObjectGroup :: [Char] -> ObjectManager t -> [ObjectManager t] -> [ObjectManager t]
replaceObjectGroup name m [] = []
replaceObjectGroup name m (x:xs) = if name == getObjectManagerName x then m:xxs else x:xxs where
                                        xxs = replaceObjectGroup name m xs

getCurrentShipIndex :: Int -> SGame Int
getCurrentShipIndex playerId = do
    attr <- getGameAttribute
    manager <- findObjectManager (spaceshipManagerNames !! playerId)
    let ships = (getObjectManagerObjects manager)
    return $ currentShip (players attr !! playerId) `mod` (length ships)

getCurrentShip :: Int -> SGame (GameObject ObjectAttributes)
getCurrentShip playerId = do
    shipId <- getCurrentShipIndex playerId
    manager <- findObjectManager (spaceshipManagerNames !! playerId)
    let ships = (getObjectManagerObjects manager)
    return $ ships !! shipId

playerHasShips :: Int -> SGame Bool
playerHasShips playerId = do
    manager <- findObjectManager (spaceshipManagerNames !! playerId)
    return $ (not $ null $ getObjectManagerObjects manager)

whenPlayerHasShips :: Int -> SGame () -> SGame ()
whenPlayerHasShips playerId m = playerHasShips playerId >>= \b -> when b m

rotateCurrentShip :: Int -> Double -> Modifiers -> Position -> SGame ()
rotateCurrentShip playerId angleDiff modifiers position =
    whenPlayerHasShips playerId $ getCurrentShip playerId >>= rotateShip angleDiff

rotateShip :: Double -> (GameObject ObjectAttributes) -> SGame ()
rotateShip angleDiff ship = do
    attr <- getObjectAttribute ship
    setObjectAttribute (attr { angle = (angle attr) + angleDiff }) ship

accelerateCurrentShip :: Int -> Double -> Modifiers -> Position -> SGame ()
accelerateCurrentShip playerId speedDiff modifiers position = whenPlayerHasShips playerId $ do
    ship <- getCurrentShip playerId
    ShipAttributes angle ownerId _ <- getObjectAttribute ship
    (vx, vy) <- getObjectSpeed ship
    let noseAngle = angle + atan (vy / vx) + (if signum vx == -1 then pi else 0) --todo generify angle calculation
    let (dvx, dvy) = (cos noseAngle * speedDiff, sin noseAngle * speedDiff)
    setObjectSpeed (vx + dvx, vy + dvy) ship

switchToNextShip :: Int -> Modifiers -> Position -> SGame ()
switchToNextShip playerId _ _ = do
    currentShipId <- getCurrentShipIndex playerId
    let newShipId = currentShipId + 1
    attr <- getGameAttribute
    let player = (players attr) !! playerId
    setGameAttribute $ attr { players = (players attr) & element playerId .~ (player { currentShip = newShipId })  }

nextBulletId :: SGame Int
nextBulletId = do
    attr <- getGameAttribute
    let nextId = 1 + bulletId attr
    setGameAttribute $ attr { bulletId = nextId }
    return nextId

_shotSpeed = 3.0
_shotLifetime = 250
_shotSafeTime = 30

shoot :: Int -> Modifiers -> Position -> SGame ()
shoot playerId _ _ = whenPlayerHasShips playerId $ do
    currentShip <- getCurrentShip playerId
    p <- getObjectPosition currentShip
    (vx, vy) <- getObjectSpeed currentShip
    attr <- getObjectAttribute currentShip
    when (shotCooldown attr <= 0) $ do
        let nose = noseAngle (vx, vy) (angle attr)
        let (nx, ny) = (cos nose, sin nose)
        let speed = (vx + nx * _shotSpeed, vy + ny * _shotSpeed) --todo rewrite
        bulletId <- nextBulletId
        let Color r g b = shotColors !! playerId
        shipName <- getObjectName currentShip
        let bullet = object ("bullet" ++ show bulletId) (Basic $ Circle 1.5 r g b Filled)
                      False (p +++ speed) speed (BulletAttributes _shotLifetime shipName)
        managers <- getObjectManagers
        setObjectAttribute (attr { shotCooldown = _shotCooldown }) currentShip
        addObjectsToGroup [bullet] bulletsManagerName

performGravity :: SGame ()
performGravity = forM_ (bulletsManagerName:spaceshipManagerNames) (\managerName -> do
        manager <- findObjectManager managerName
        let ships = getObjectManagerObjects manager
        forM_ ships handleGravity
    )

handleGravity :: (GameObject ObjectAttributes) -> SGame ()
handleGravity object = do
    planet <- findObject "planet" "planetGroup"
    (px, py) <- getObjectPosition planet
    (vx, vy) <- getObjectSpeed object
    (sx, sy) <- getObjectPosition object
    let (dx, dy) = diff (px, py) (sx, sy)
    let r = distance (px, py) (sx, sy)
    let acceleration = (accelerationValue r) *** (ort (px, py) (sx, sy))
    setObjectSpeed ((vx , vy) +++ acceleration) object

performPlanetCollision :: SGame ()
performPlanetCollision = do
    planet <- findObject "planet" "planetGroup"
    (px, py) <- getObjectPosition planet
    forM_ (bulletsManagerName:spaceshipManagerNames) (\managerName -> do
        manager <- findObjectManager managerName
        let ships = getObjectManagerObjects manager
        forM_ ships (\ship -> do
            (x, y) <- getObjectPosition ship
            let r = distance (px, py) (x, y)
            when (r < planetRadius) $ destroyObject ship
         )
     )

handleCooldowns :: SGame ()
handleCooldowns = do
    forM_ spaceshipManagerNames $ (\n -> do
        manager <- findObjectManager n
        forM_ (getObjectManagerObjects manager) $ (\ship -> do
            attr <- getObjectAttribute ship
            setObjectAttribute (attr { shotCooldown = (shotCooldown attr) - 1 }) ship
         )
     )

handleShotLifetimes :: SGame ()
handleShotLifetimes = do
    manager <- findObjectManager bulletsManagerName
    forM_ (getObjectManagerObjects manager) $ (\bullet -> do
        attr <- getObjectAttribute bullet
        let l = (lifetime attr) - 1
        if l < 0 then destroyObject bullet else
            setObjectAttribute (attr { lifetime = l }) bullet
     )

handleHits :: SGame ()
handleHits = do
    bulletManager <- findObjectManager bulletsManagerName
    forM_ (getObjectManagerObjects bulletManager) (\bullet -> do
        (bx, by) <- getObjectPosition bullet
        attr <- getObjectAttribute bullet
        forM_ (spaceshipManagerNames) (\n -> do
            shipManager <- findObjectManager n
            forM_ (getObjectManagerObjects shipManager) (\ship -> do
                shipName <- getObjectName ship
                (px, py) <- getObjectPosition ship
                let d = distance (bx, by) (px, py)
                when (d < _triangleRadius &&
                    (shipName /= (fromShipName attr) ||
                    (lifetime attr < _shotLifetime - _shotSafeTime))) $ do
                    destroyObject bullet
                    destroyObject ship
             )
         )
     )

gameCycle :: SGame ()
gameCycle = do
  performGravity
  updateShipPictures
  performPlanetCollision
  handleCooldowns
  handleHits
  handleShotLifetimes