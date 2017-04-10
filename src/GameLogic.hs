{-# LANGUAGE TemplateHaskell #-}

module GameLogic where

import Control.Lens
import Data.List
import GameTypes
import GameUtils
import GameStrings
import GameParameters
import Geometry
import Physics
import Control.Monad
import Control.Monad.Loops
import Graphics.UI.Fungen hiding (when)

-- |The 'makeShot' function performs a shot if the recharging time is already spent and sets a new counter for this time,
-- otherwise it does nothing.
makeShot :: Int ->  SGame ()
makeShot playerId = whenRunningGame $ whenPlayerHasShips playerId $ do
    currentShip <- getCurrentShip playerId
    p <- getObjectPosition currentShip
    (vx, vy) <- getObjectSpeed currentShip
    attr <- getObjectAttribute currentShip
    when (_shotCooldown attr <= 0) $ do
        let nose = noseAngle (vx, vy) (_angle attr)
        let (nx, ny) = (cos nose, sin nose)
        let speed = (vx, vy) +++ (shotSpeed *** (nx, ny))
        bulletId <- nextBulletId
        let Color r g b = shotColors !! playerId
        shipName <- getObjectName currentShip
        let bullet = object ("bullet" ++ show bulletId) (Basic $ Circle 1.5 r g b Filled)
                      False (p +++ speed) speed (BulletAttributes shotLifetime shipName)
        managers <- getObjectManagers
        setObjectAttribute (shotCooldown .~ shotCooldownConst $ attr) currentShip
        addObjectsToGroup [bullet] bulletsManagerName

-- |The 'updateShipPictures' function updates ships' coordinates and orientation.
updateShipPictures :: SGame ()
updateShipPictures = do
    managers <- getObjectManagers
    newManagers <- forM (zip [0..] spaceshipManagerNames) $ \(playerId, managerName) -> do
        let manager = searchObjectManager managerName managers
        let objects = getObjectManagerObjects manager
        currentShipIndex <- getCurrentShipIndex playerId
        newObjects <- forM (zip [0..] objects) $ \(shipId, o) -> do
            name <- getObjectName o
            position <- getObjectPosition o
            speed <- getObjectSpeed o
            a@(ShipAttributes angle ownerId _) <- getObjectAttribute o
            return $ object name (shipPoly speed angle ownerId triangleRadius (shipId == currentShipIndex)) False position speed a
        return $ objectGroup managerName newObjects
    setObjectManagers $ foldr (\a b -> replaceObjectGroup (getObjectManagerName a) a b) managers newManagers

-- |The 'performGravity' function applies the 'handleGravity' function for each spaceship.
performGravity :: SGame ()
performGravity = forM_ (bulletsManagerName:debrisManagerName:spaceshipManagerNames) $ \managerName -> do
    manager <- findObjectManager managerName
    let ships = getObjectManagerObjects manager
    forM_ ships handleGravity

-- |The 'handleGravity' function makes ship move accordingly to the low F = G*m1*m2/r^2. G*m1*m2 is not really
-- calculated, the constant 'gravity' from the 'GameParameters' module can be tuned instead.
handleGravity :: GameObject ObjectAttributes -> SGame ()
handleGravity object = do
    planet <- findObject "planet" "planetGroup"
    (px, py) <- getObjectPosition planet
    (vx, vy) <- getObjectSpeed object
    (sx, sy) <- getObjectPosition object
    let (dx, dy) = diff (px, py) (sx, sy)
    let r = distance (px, py) (sx, sy)
    let acceleration = accelerationValue r *** ort (px, py) (sx, sy)
    setObjectSpeed ((vx , vy) +++ acceleration) object

-- |The 'performPlanetCollision' function makes the ship collided with the planet disappear from the game.
performPlanetCollision :: SGame ()
performPlanetCollision = do
    planet <- findObject "planet" "planetGroup"
    (px, py) <- getObjectPosition planet
    forM_ (bulletsManagerName:debrisManagerName:spaceshipManagerNames) $ \managerName -> do
        manager <- findObjectManager managerName
        let ships = getObjectManagerObjects manager
        forM_ ships $ \ship -> do
            (x, y) <- getObjectPosition ship
            let r = distance (px, py) (x, y)
            when (r < planetRadius) $ destroyObject ship

-- |The 'handleCooldowns' function decrements the time which is needed to make the next shot possible.
handleCooldowns :: SGame ()
handleCooldowns =
    forM_ spaceshipManagerNames $ \n -> do
        manager <- findObjectManager n
        forM_ (getObjectManagerObjects manager) $ \ship -> do
            attr <- getObjectAttribute ship
            setObjectAttribute (attr { _shotCooldown = _shotCooldown attr - 1 }) ship

-- |The 'handleLifetimes' function provides destroying of the bullets, debris and explosions after '_lifetime' is spent.
-- The function decrements the '_lifetime' for each object and then destroys objects with the non-positive '_lifetime'.
handleLifetimes :: SGame ()
handleLifetimes = forM_ [bulletsManagerName, debrisManagerName, explosionsManagerName] $ \managerName -> do
    manager <- findObjectManager managerName
    forM_ (getObjectManagerObjects manager) $ \bullet -> do
        attr <- getObjectAttribute bullet
        let l = _lifetime attr - 1
        if l < 0 then destroyObject bullet else
            setObjectAttribute (lifetime .~ l $ attr) bullet

-- |The 'createDebris' function makes debris appear on the screen when the ship is hit by bullet.
createDebris :: Int -> Point -> Vector -> SGame ()
createDebris playerId (px, py) (vx, vy) = do
    nDebris <- randomInt debrisPiecesNumber
    objects <- forM [1..nDebris] $ \_ -> do
        objectId <- nextBulletId
        size <- randomDouble debrisRadius
        dpx <- randomDouble debrisShift
        dpy <- randomDouble debrisShift
        rangle <- randomDouble debrisSpeedSpread
        let v = distance (0,0) (vx, vy)
        rv <- randomDouble (v * 0.8, v * 1.2)
        let vangle = noseAngle (vx, vy) 0.0 + rangle
        time <- randomInt debrisLifetime
        let pos = (px + dpx, py + dpy)
        let speed = (rv * cos vangle, rv * sin vangle)
        return $ object ("debris" ++ show objectId) (shipPoly pos rangle playerId size False)
                                False pos speed (BulletAttributes time "")
    addObjectsToGroup objects debrisManagerName

-- |The 'createExplosion' function makes explosion happen when the ship is hit by bullet.
createExplosion :: Int -> Point -> Vector -> SGame ()
createExplosion playerId p (vx, vy) = do
    manager <- findObjectManager explosionsManagerName
    objects <- forM [1..explosionParticles] $ \_ -> do
        objectId <- nextBulletId
        dpx <- randomDouble explosionRadius
        dpy <- randomDouble explosionRadius
        angle <- randomDouble (0, 2 * pi)
        dv <- randomDouble explosionSpeed
        let (dvx, dvy) = (dv * cos angle, dv * sin angle)
        er <- randomDouble explosionRadius
        let Color r g b = playerColors !! playerId
        return $ object ("explosion" ++ (show objectId)) (Basic (Circle er r g b Filled))
                                        False p (vx + dvx, vy + dvy) (BulletAttributes explosionTime "")
    addObjectsToGroup objects debrisManagerName

-- |The 'handleHits' function does all of the stuff which must happen after the spaceship is hit. It switches the
-- player' selected ship to the next one, destroys the ship object and calls 'createExplosion', 'createDebris' functions.
-- It also destroys the bullet if there was a hit.
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
                if d < triangleRadius &&
                    (shipName /= (attr^.fromShipName) ||
                    (_lifetime attr < shotLifetime - shotSafeTime)) then do
                    if isPlayer then do
                        currentShipName <- getObjectName =<< getCurrentShip playerId
                        destroyObject o
                        let impulse = addImpulse 0.8 0.2 (vx, vy) (bvx, bvy)
                        createDebris playerId p impulse
                        createExplosion playerId p impulse
                        if shipName == currentShipName then switchToNextShip 1 playerId else do
                            objs <- getObjectManagerObjects <$> findObjectManager targetManager
                            names <- mapM getObjectName objs
                            let Just i = elemIndex currentShipName names
                            setCurrentShipIndex playerId i
                        else destroyObject o
                    return True
                 else
                    return False)
        hasHit <- (not . null) . concat <$> mapM check ((debrisManagerName, -1):zip spaceshipManagerNames [0..])
        when hasHit $ destroyObject bullet

-- |The 'checkEndGame' function checks the condition: are there any ships of the player on the screen now, if not the
-- player lost game or there is a draw if the second player also does not have ships on the screen. It calls the 'endGame'
-- function if needed and passes winner/winners' names.
checkEndGame :: SGame ()
checkEndGame = do
     let nameIds = zip spaceshipManagerNames [0..]
     winners <- filterM (\(_, i) -> playerHasShipsInBound i) nameIds
     when (length winners < length nameIds) $ endGame (map snd winners)

-- |The 'gameIteration' function calls all of the functions above one-by-one to maintain the right state of the game.
gameIteration :: SGame ()
gameIteration = do
    performGravity
    updateShipPictures
    handleCooldowns
    handleHits
    handleLifetimes
    performPlanetCollision
    checkEndGame