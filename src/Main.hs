module Main where

import Graphics.UI.Fungen hiding (when)
import Control.Lens
import Data.List (findIndex)
import Graphics.UI.Fungen.Objects
import Graphics.Rendering.OpenGL (GLdouble)
import Control.Monad
import Control.Monad.Loops
import Geometry
import Physics

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
playerKeys = [(SpecialKey KeyRight, SpecialKey KeyLeft, SpecialKey KeyUp, Char '.', SpecialKey KeyDown),
              (Char 'd', Char 'a', Char 'w', Char 'q', Char 's')]

spaceshipManagerNames = ["ships1", "ships2"]

bulletsManagerName = "bullets"
bulletsGroup = objectGroup bulletsManagerName []

planetRadius = 80.0
planetManagerName = "planetGroup"

planetGroup = objectGroup planetManagerName [planet]

debrisManagerName = "debrisGroup"
debrisGroup = objectGroup debrisManagerName []

planet :: GameObject ObjectAttributes
planet =
  let planetPic = Basic (Circle planetRadius 1.0 0.0 0.0 Filled)
  in object "planet" planetPic False (w/2,h/2) (0,0) NoAttributes

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
              createInput (player, (r, l, u, n, s)) = [
                (r, StillDown, rotateCurrentShip player (-0.1)),
                (l, StillDown, rotateCurrentShip player 0.1),
                (u, StillDown, accelerateCurrentShip player 0.04),
                (n, Press, switchToNextShip player),
                (s, Press, shoot player)
               ]
      initialAttrs = GameAttribute (map (\_ -> Player 0) [0, 1]) 0
  funInit winConfig gameMap [planetGroup, bulletsGroup, debrisGroup, spaceships1, spaceships2] () initialAttrs input (gameCycle) (Timer 30) []

_shotCooldown = 15 --todo revertm testing purposes

createSpaceship :: Int -> Int -> Color -> Point -> GameObject ObjectAttributes
createSpaceship ownerId shipId color pos =
  let Color r g b = color
      speed = (0, case ownerId of 0 -> 1.4; 1 -> -1.4)
      angle = 0.0
      spaceshipPic = shipPoly speed angle ownerId _triangleRadius False
  in object ("ship" ++ (show ownerId) ++ (show shipId)) spaceshipPic False pos speed (ShipAttributes angle ownerId _shotCooldown)

_triangleSpread = pi / 4
_triangleRadius = 8.0

noseAngle :: Vector -> Double -> Double
noseAngle (vx, vy) angle =
    let vangle = atan (vy / vx) + (if signum vx == -1 then pi else 0)
    in vangle + angle

shipTriangle :: Vector -> Double -> Double -> [Point2D]
shipTriangle (vx, vy) angle size =
    let nose = noseAngle (vx, vy) angle
        angles = [nose, nose + pi - _triangleSpread, nose + pi + _triangleSpread]
    in map (\a -> (((cos a) * size), (sin a) * size)) angles

shipPoly :: Vector -> Double -> Int -> Double -> Bool -> ObjectPicture
shipPoly speed angle ownerId size isSelected =
    let Color r g b = playerColors !! ownerId in
    Basic $ Polyg (shipTriangle speed angle size) r g b (if isSelected then Filled else Unfilled)

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
            return $ object name (shipPoly speed angle ownerId _triangleRadius (shipId == currentShipIndex)) False position speed a
         )
        return $ objectGroup managerName newObjects
     )
    setObjectManagers $ foldr (\a b -> replaceObjectGroup (getObjectManagerName a) a b) managers newManagers

replaceObjectGroup :: [Char] -> ObjectManager t -> [ObjectManager t] -> [ObjectManager t]
replaceObjectGroup name m [] = []
replaceObjectGroup name m (x:xs) = if name == getObjectManagerName x then m:xxs else x:xxs where
                                        xxs = replaceObjectGroup name m xs

setCurrentShipIndex :: Int -> Int -> SGame ()
setCurrentShipIndex playerId shipIndex = do
    attr <- getGameAttribute
    let player = (players attr) !! playerId
    setGameAttribute $ attr { players = (players attr) & element playerId .~ (player { currentShip = shipIndex }) }

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

playerHasShipsInBound :: Int -> SGame Bool
playerHasShipsInBound playerId = do
    manager <- findObjectManager (spaceshipManagerNames !! playerId)
    let c o = do
        p <- getObjectPosition o
        return $ isInBound p
    shipsInBound <- filterM c $ getObjectManagerObjects manager
    return (not $ null $ shipsInBound)

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

isInBound :: Point -> Bool
isInBound (px, py) = px >= -_triangleRadius && px <= w + _triangleRadius &&
                     py >= -_triangleRadius && py <= h + _triangleRadius

switchToNextShip :: Int -> Modifiers -> Position -> SGame ()
switchToNextShip playerId _ _ = switchToNextShip' playerId

switchToNextShip' :: Int -> SGame ()
switchToNextShip' playerId = do
    canSwitch <- playerHasShipsInBound playerId
    when canSwitch $ untilM_ next inBound where
        next = do
            currentShipId <- getCurrentShipIndex playerId
            let newShipId = currentShipId + 1
            attr <- getGameAttribute
            let player = (players attr) !! playerId
            setGameAttribute $ attr { players = (players attr) & element playerId .~ (player { currentShip = newShipId }) }
        inBound = do
            currentShipId <- getCurrentShipIndex playerId
            manager <- findObjectManager $ spaceshipManagerNames !! playerId
            p <- getObjectPosition $ (getObjectManagerObjects manager) !! currentShipId
            return $ isInBound p

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
performGravity = forM_ (bulletsManagerName:debrisManagerName:spaceshipManagerNames) (\managerName -> do
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
    forM_ (bulletsManagerName:debrisManagerName:spaceshipManagerNames) (\managerName -> do
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

handleLifetimes :: SGame ()
handleLifetimes = forM_ [bulletsManagerName, debrisManagerName] $ \managerName -> do
    manager <- findObjectManager managerName
    forM_ (getObjectManagerObjects manager) $ (\bullet -> do
        attr <- getObjectAttribute bullet
        let l = (lifetime attr) - 1
        if l < 0 then destroyObject bullet else
            setObjectAttribute (attr { lifetime = l }) bullet
     )

_debrisPiecesNumber = (5, 8)
_debrisRadius = (2.0, _triangleRadius / 2.5)
_debriLifetime = (100, 200)
_debrisShift = (0, _triangleRadius)
_debrisSpeedSpread = (-pi/8, pi/8)

createDebris :: Int -> Point -> Vector -> SGame ()
createDebris playerId (px, py) (vx, vy) = do
    nDebris <- randomInt _debrisPiecesNumber
    objects <- forM [1..nDebris] $ \_ -> do
        objectId <- nextBulletId
        size <- randomDouble _debrisRadius
        dpx <- randomDouble _debrisShift
        dpy <- randomDouble _debrisShift
        rangle <- randomDouble _debrisSpeedSpread
        let v = distance (0,0) (vx, vy)
        rv <- randomDouble (v * 0.8, v * 1.2)
        let vangle = noseAngle (vx, vy) 0.0 + rangle
        time <- randomInt _debriLifetime
        let pos = (px + dpx, py + dpy)
        let speed = (rv * cos vangle, rv * sin vangle)
        return $ object ("debris" ++ (show objectId)) (shipPoly pos rangle playerId size False)
                                False pos speed (BulletAttributes time "")
    addObjectsToGroup objects debrisManagerName

_explosionTime = 5
_explosionMaxRadius = _triangleRadius

--createExplosion :: Int -> Point -> Vector -> SGame ()
--createExplosion
handleHits :: SGame ()
handleHits = do
    bulletManager <- findObjectManager bulletsManagerName
    forM_ (getObjectManagerObjects bulletManager) $ \bullet -> do
        (bx, by) <- getObjectPosition bullet
        (bvx, bvy) <- getObjectSpeed bullet
        attr <- getObjectAttribute bullet
        let check (targetManager, playerId) = do
            let isPlayer = playerId >= 0
            shipManager <- findObjectManager targetManager
            flip filterM (getObjectManagerObjects shipManager) (\o -> do
                shipName <- getObjectName o
                p <- getObjectPosition o
                (vx, vy) <- getObjectSpeed o
                let d = distance (bx, by) p
                if (d < _triangleRadius &&
                    (shipName /= (fromShipName attr) ||
                    (lifetime attr < _shotLifetime - _shotSafeTime))) then do
                    if isPlayer then do
                        currentShipName <- getObjectName =<< getCurrentShip playerId
                        destroyObject o
                        createDebris playerId p (vx * 0.8 + bvx * 0.2, vy * 0.8 + bvy * 0.2)
                        if shipName == currentShipName then switchToNextShip' playerId else do
                            objs <- getObjectManagerObjects <$> findObjectManager targetManager
                            names <- mapM getObjectName objs
                            let Just i = findIndex (\x -> x == currentShipName) names
                            setCurrentShipIndex playerId i
                        else do destroyObject o
                    return True
                 else do
                    return False)
        hasHit <- (not . null) <$> concat <$> (sequence $ map check $ (debrisManagerName, -1):(zip spaceshipManagerNames [0..]))
        when hasHit $ destroyObject bullet

gameCycle :: SGame ()
gameCycle = do
  performGravity
  updateShipPictures
  handleCooldowns
  handleHits
  handleLifetimes
  performPlanetCollision