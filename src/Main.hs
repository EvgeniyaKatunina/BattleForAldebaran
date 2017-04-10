{-# LANGUAGE TemplateHaskell #-}

module Main where

import Graphics.UI.Fungen (funInit, WindowConfig, RefreshType(Timer))
import GameParameters (width, height, frameTime)
import GameTypes
import GameUtils (gameMap, initialAttrs, createGroups)
import GameWrapper (input, gameCycle)
import GameStrings

winConfig :: WindowConfig
winConfig = ((100, 80), (fromIntegral $ round width, fromIntegral $ round height), gameName)

main :: IO ()
main = funInit winConfig gameMap (createGroups initialAttrs) () initialAttrs input gameCycle (Timer frameTime) []