{-# LANGUAGE TemplateHaskell #-}

module GameTypes where

import Graphics.UI.Fungen
import Control.Lens

data Color = Color
  { _red :: Float
  , _green :: Float
  , _blue :: Float
  }

data Player = Player
  { _currentShip :: Int
  }

data GameAttribute = GameAttribute
  { _phase :: GamePhase
  , _players :: [Player]
  , _bulletId :: Int
  , _nShips :: Int
  }

data ObjectAttributes = NoAttributes
                      | ShipAttributes
                        { _angle :: Double
                        , _ownerId :: Int
                        , _shotCooldown :: Int
                        }
                      | BulletAttributes
                        { _lifetime :: Int
                        , _fromShipName :: String
                        }

data GamePhase = BeforeStart
                 | Running
                 | AfterEnd [Int] deriving Eq

type SGame a = IOGame GameAttribute ObjectAttributes () () a

makeLenses ''GameAttribute
makeLenses ''Player
makeLenses ''Color
makeLenses ''ObjectAttributes