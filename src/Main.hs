{-# LANGUAGE TemplateHaskell #-}

module Main where

import Graphics.UI.Fungen (funInit, WindowConfig, RefreshType(Timer))
import GameParameters
import GameTypes
import GameLogic
import GameUtils
import GameWrapper
import GameStrings

winConfig :: WindowConfig
winConfig = ((100, 80), (fromIntegral $ round width, fromIntegral $ round height), gameName)

main :: IO ()
main = funInit winConfig gameMap (createGroups initialAttrs) () initialAttrs input gameCycle (Timer frameTime) []