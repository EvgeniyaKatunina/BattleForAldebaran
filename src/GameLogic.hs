{-# LANGUAGE TemplateHaskell #-}

module GameLogic where

import Control.Lens ((.~), (^.))
import Data.List (elemIndex)
import GameTypes
import GameUtils (spaceshipManagerNames, playerHasShipsInBound, getCurrentShipIndex,
                   setCurrentShipIndex, getCurrentShip, switchToNextShip, debrisManagerName,
                   endGame, bulletsManagerName, nextBulletId, explosionsManagerName, shipPoly,
                   whenRunningGame, whenPlayerHasShips, playerColors, shotColors, replaceObjectGroup)
import GameStrings
import GameParameters
import Geometry
import Physics
import Control.Monad (forM_, forM, when, filterM)
import Graphics.UI.Fungen hiding (when)

-- |Performs a shot if the recharging time is already spent and sets a new counter for this time,
-- otherwise does nothing.
makeShot :: Int ->  SGame ()
makeShot playerId = whenRunningGame $ whenPlayerHasShips playerId $ do
    currentShip <- getCurrentShip playerId
    Point pos <- Point <$> getObjectPosition currentShip
    v@(Vector (vx, vy)) <- Vector <$> getObjectSpeed currentShip
    attr <- getObjectAttribute currentShip
    when (_shotCooldown attr <= 0) $ do
        let nose = noseAngle v (_angle attr)
        let n = Vector (cos nose, sin nose)
        let Vector speed = v ^+^ (shotSpeed #*^ n)
        bulletId <- nextBulletId
        let Color r g b = shotColors !! playerId
        shipName <- getObjectName currentShip
        let bullet = object ("bullet" ++ show bulletId) (Basic $ Circle 1.5 r g b Filled)
                      False pos speed (BulletAttributes shotLifetime shipName)
        managers <- getObjectManagers
        setObjectAttribute (shotCooldown .~ shotCooldownConst $ attr) currentShip
        addObjectsToGroup [bullet] bulletsManagerName

-- |Updates ships' coordinates and orientation.
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
            sv@(Vector speed) <- Vector <$> getObjectSpeed o
            a@(ShipAttributes angle ownerId _) <- getObjectAttribute o
            return $ object name (shipPoly sv angle ownerId triangleRadius (shipId == currentShipIndex)) False position speed a
        return $ objectGroup managerName newObjects
    setObjectManagers $ foldr (\a b -> replaceObjectGroup (getObjectManagerName a) a b) managers newManagers

-- |Applies the 'handleGravity' function for each spaceship.
performGravity :: SGame ()
performGravity = forM_ (bulletsManagerName:debrisManagerName:spaceshipManagerNames) $ \managerName -> do
    manager <- findObjectManager managerName
    let ships = getObjectManagerObjects manager
    forM_ ships handleGravity

-- |Makes ship move accordingly to the low F = G*m1*m2/r^2. G*m1*m2 is not really
-- calculated, the constant 'gravity' from the 'GameParameters' module can be tuned instead.
handleGravity :: GameObject ObjectAttributes -> SGame ()
handleGravity object = do
    planet <- findObject "planet" "planetGroup"
    p <- Point <$> getObjectPosition planet
    v <- Vector <$> getObjectSpeed object
    s <- Point <$> getObjectPosition object
    let Vector (dx, dy) = diff p s
    let r = distance p s
    let acceleration = accelerationValue r #*^ ort p s
    let Vector rv = v ^+^ acceleration
    setObjectSpeed rv object

-- |Makes the ship collided with the planet disappear from the game.
performPlanetCollision :: SGame ()
performPlanetCollision = do
    planet <- findObject "planet" "planetGroup"
    (px, py) <- getObjectPosition planet
    forM_ (bulletsManagerName:debrisManagerName:spaceshipManagerNames) $ \managerName -> do
        manager <- findObjectManager managerName
        let ships = getObjectManagerObjects manager
        forM_ ships $ \ship -> do
            (x, y) <- getObjectPosition ship
            let r = distance (Point (px, py)) (Point (x, y))
            when (r < planetRadius) $ destroyObject ship

-- |Decrements the time which is needed to make the next shot possible.
handleCooldowns :: SGame ()
handleCooldowns =
    forM_ spaceshipManagerNames $ \n -> do
        manager <- findObjectManager n
        forM_ (getObjectManagerObjects manager) $ \ship -> do
            attr <- getObjectAttribute ship
            setObjectAttribute (attr { _shotCooldown = _shotCooldown attr - 1 }) ship

-- |Provides destroying of the bullets, debris and explosions after '_lifetime' is spent.
-- The function decrements the '_lifetime' for each object and then destroys objects with the non-positive '_lifetime'.
handleLifetimes :: SGame ()
handleLifetimes = forM_ [bulletsManagerName, debrisManagerName, explosionsManagerName] $ \managerName -> do
    manager <- findObjectManager managerName
    forM_ (getObjectManagerObjects manager) $ \bullet -> do
        attr <- getObjectAttribute bullet
        let l = _lifetime attr - 1
        if l < 0 then destroyObject bullet else
            setObjectAttribute (lifetime .~ l $ attr) bullet

-- |Makes debris appear on the screen when the ship is hit by bullet. The Int argument is
-- for the player which spaceship was hit, Point, Vector arguments determine where and with which speed the debris must
-- appear.
createDebris :: Int -> Point -> Vector -> SGame ()
createDebris playerId p@(Point (px, py)) v@(Vector (vx, vy)) = do
    nDebris <- randomInt debrisPiecesNumber
    objects <- forM [1..nDebris] $ \_ -> do
        objectId <- nextBulletId
        size <- randomDouble debrisRadius
        dpx <- randomDouble debrisShift
        dpy <- randomDouble debrisShift
        rangle <- randomDouble debrisSpeedSpread
        let sv = distance (Point (0,0)) (Point (0, 0) .+^ v)
        rv <- randomDouble (sv * 0.8, sv * 1.2)
        let vangle = noseAngle v 0.0 + rangle
        time <- randomInt debrisLifetime
        let pos = (px + dpx, py + dpy)
        let speed = (rv * cos vangle, rv * sin vangle)
        return $ object ("debris" ++ show objectId) (shipPoly (Vector (dpx, dpy)) rangle playerId size False)
                                False pos speed (BulletAttributes time "")
    addObjectsToGroup objects debrisManagerName

-- |Makes explosion happen when the ship is hit by bullet. The Int argument is
-- for the player which spaceship was hit, Point, Vector arguments determine where and with which speed the debris must
-- appear.
createExplosion :: Int -> Point -> Vector -> SGame ()
createExplosion playerId (Point (px, py)) v@(Vector (vx, vy)) = do
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
                                        False (px + dpx, py + dpy) (vx + dvx, vy + dvy) (BulletAttributes explosionTime "")
    addObjectsToGroup objects debrisManagerName

-- |Does all of the stuff which must happen after the spaceship is hit. It switches the
-- player' selected ship to the next one, destroys the ship object and calls 'createExplosion', 'createDebris' functions.
-- It also destroys the bullet if there was a hit.
handleHits :: SGame ()
handleHits = do
    bulletManager <- findObjectManager bulletsManagerName
    forM_ (getObjectManagerObjects bulletManager) $ \bullet -> do
        (bx, by) <- getObjectPosition bullet
        bv <- Vector <$> getObjectSpeed bullet
        attr <- getObjectAttribute bullet
        let check (targetManager, playerId) = do
                                                let isPlayer = playerId >= 0
                                                shipManager <- findObjectManager targetManager
                                                flip filterM (getObjectManagerObjects shipManager) (\o -> do
                                                    shipName <- getObjectName o
                                                    p <- Point <$> getObjectPosition o
                                                    v <- Vector <$> getObjectSpeed o
                                                    let d = distance (Point (bx, by)) p
                                                    if d < triangleRadius &&
                                                        (shipName /= (attr^.fromShipName) ||
                                                        (_lifetime attr < shotLifetime - shotSafeTime)) then do
                                                        if isPlayer then do
                                                            currentShipName <- getObjectName =<< getCurrentShip playerId
                                                            destroyObject o
                                                            let impulse = addImpulse 0.8 0.2 v bv
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

-- |Checks the condition: are there any ships of the player on the screen now, if not the
-- player lost game or there is a draw if the second player also does not have ships on the screen. It calls the 'endGame'
-- function if needed and passes winner/winners' names.
checkEndGame :: SGame ()
checkEndGame = do
     let nameIds = zip spaceshipManagerNames [0..]
     winners <- filterM (\(_, i) -> playerHasShipsInBound i) nameIds
     when (length winners < length nameIds) $ endGame (map snd winners)

-- |Calls all of the functions above one-by-one to maintain the right state of the game.
gameIteration :: SGame ()
gameIteration = do
    performGravity
    updateShipPictures
    handleCooldowns
    handleHits
    handleLifetimes
    performPlanetCollision
    checkEndGame