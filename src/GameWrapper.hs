{-# LANGUAGE TemplateHaskell #-}

module GameWrapper where

import Control.Lens ((.~), (^.))
import GameTypes
import GameStrings
import GameParameters
import GameUtils (createGroups, endGame, getCurrentShip,
                   whenPlayerHasShips, whenRunningGame, rotateShip, accelerateShip,
                   printLines, switchToNextShip)
import Graphics.UI.Fungen.Objects
import Graphics.UI.Fungen hiding (when)
import GameLogic (makeShot, gameIteration)

playerKeys :: [(Key, Key, Key, Key, Key, Key)]
playerKeys = [(SpecialKey KeyRight, SpecialKey KeyLeft, SpecialKey KeyUp, Char '.', Char ',', SpecialKey KeyDown),
              (Char 'd', Char 'a', Char 'w', Char 'q', Char 'e', Char 's')]

input :: [(Key, KeyEvent, Modifiers -> Position -> SGame ())]
input = [(Char ' ', Press, startGame), (SpecialKey KeyF2, Press, exitGame)] ++
        map (\i -> (Char (head (show i)), Press, setNShips i)) [1..9] ++
        concatMap createInput (zip [0..] playerKeys) where
              createInput (player, (r, l, u, n, p, s)) = [
                (r, StillDown, rotateCurrentShip player (-rotationSpeed)),
                (l, StillDown, rotateCurrentShip player rotationSpeed),
                (u, StillDown, accelerateCurrentShip player acceleration),
                (n, Press, changeShip 1 player),
                (p, Press, changeShip (-1) player),
                (s, Press, shoot player)]

gameCycle :: SGame ()
gameCycle = do
    attr <- getGameAttribute
    case attr^.phase of
      Running -> gameIteration
      BeforeStart -> do
          printLines ([gameName, ""] ++ gameIntro ++ "":shipsSelection (attr^.nShips)) Fixed9By15 17 (5, height - 17) (Color 1.0 0.0 0.0)
          printLines controlsInfo Fixed9By15 (-17) (5, 17) (Color 1.0 0.0 0.0)
      AfterEnd ids -> do
          let winnerStr = case ids of [a] -> winnerMessage a; _ -> drawMessage
          printLines [gameFinishedStr, winnerStr] Fixed9By15 17 (5, height - 17) (Color 1.0 0.0 0.0)
          printLines restartInfo Fixed9By15 (-17) (5, 17) (Color 1.0 0.0 0.0)

rotateCurrentShip :: Int -> Double -> Modifiers -> Position -> SGame ()
rotateCurrentShip playerId angleDiff modifiers position =
    whenRunningGame $ whenPlayerHasShips playerId $ getCurrentShip playerId >>= rotateShip angleDiff

accelerateCurrentShip :: Int -> Double -> Modifiers -> Position -> SGame ()
accelerateCurrentShip playerId speedDiff modifiers position = whenRunningGame $ whenPlayerHasShips playerId $ do
    ship <- getCurrentShip playerId
    accelerateShip speedDiff ship

shoot :: Int -> Modifiers -> Position -> SGame ()
shoot playerId _ _ = makeShot playerId

startGame :: Modifiers -> Position -> SGame ()
startGame _ _ = do
    attr <- getGameAttribute
    case attr^.phase of
        BeforeStart -> do
            let a = phase .~ Running $ attr
            setObjectManagers $ createGroups a
            setGameAttribute a
        AfterEnd _ -> do
            let a = phase .~ BeforeStart $ attr
            setObjectManagers $ createGroups a
            setGameAttribute a
        Running -> return ()

exitGame :: Modifiers -> Position -> SGame ()
exitGame _ _ = do
    attr <- getGameAttribute
    case attr^.phase of
        BeforeStart -> funExit
        AfterEnd _ -> funExit
        Running -> endGame []

setNShips :: Int -> Modifiers -> Position -> SGame ()
setNShips n _ _ = do
    attr <- getGameAttribute
    case attr^.phase of
        BeforeStart ->
            setGameAttribute $ nShips .~ n $ attr
        _ -> return ()

changeShip :: Int -> Int -> Modifiers -> Position -> SGame ()
changeShip nextPrev playerId _ _ = switchToNextShip nextPrev playerId
