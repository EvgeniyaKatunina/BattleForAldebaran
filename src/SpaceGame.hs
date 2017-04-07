module SpaceGame where

import Graphics.UI.Fungen
import Graphics.UI.Fungen.Objects
import Graphics.Rendering.OpenGL (GLdouble)
import Geometry
import Control.Monad
import Physics

data GameAttribute = Score Int

width = 400
height = 400
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble

nShips = 3

planet = objectGroup "planetGroup" [createPlanet]

spaceships1 = objectGroup "ships1" $
                  map (\i -> createSpaceship ("spaceship1_" ++ (show i)) (Color 0.0 1.0 0.0) (w/2 + 50 + 5 * i, h/2))
                      [1..nShips]

spaceships2 = objectGroup "ships2" $
                  map (\i -> createSpaceship ("spaceship2_" ++ (show i)) (Color 0.0 0.0 1.0) (w/2 - 50 - 5 * i, h/2 + 30))
                      [1..nShips]

main :: IO ()
main = do
  let winConfig = ((100,80),(width,height),"Space game")
      gameMap = colorMap 0 0 0 w h
      {-input = [
        (SpecialKey KeyRight, StillDown, moveBarToRight)
        ,(SpecialKey KeyLeft,  StillDown, moveBarToLeft)
        ,(Char 'q',            Press,     \_ _ -> funExit)
        ]-}
  funInit winConfig gameMap [planet, spaceships1, spaceships2] () (()) [] (gameCycle) (Timer 30) []

createPlanet :: GameObject ()
createPlanet =
  let planetPic = Basic (Circle 20.0 1.0 0.0 0.0 Filled)
  in object "planet" planetPic False (w/2,h/2) (0,0) ()

data Color = Color { red :: Float, green :: Float, blue :: Float }
type Name = String

createSpaceship :: Name -> Color -> Point -> GameObject ()
createSpaceship name color pos =
  let Color r g b = color in
  let spaceshipPic = Basic (Circle 2.0 r g b Filled)
  in object name spaceshipPic False pos (0, 1.0) ()

{-moveBarToRight :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveBarToRight _ _ = do
  obj     <- findObject "bar" "barGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX + (sX/2) + 5 <= w)
   then (setObjectPosition ((pX + 5),pY) obj)
   else (setObjectPosition ((w - (sX/2)),pY) obj)

moveBarToLeft :: Modifiers -> Position -> IOGame GameAttribute () () () ()
moveBarToLeft _ _ = do
  obj <- findObject "bar" "barGroup"
  (pX,pY) <- getObjectPosition obj
  (sX,_)  <- getObjectSize obj
  if (pX - (sX/2) - 5 >= 0)
    then (setObjectPosition ((pX - 5),pY) obj)
    else (setObjectPosition (sX/2,pY) obj)-}

handleGravity :: (GameObject ()) -> IOGame () () () () ()
handleGravity ship = do
    planet <- findObject "planet" "planetGroup"
    (px, py) <- getObjectPosition planet

    (vx, vy) <- getObjectSpeed ship
    (sx, sy) <- getObjectPosition ship
    let (dx, dy) = diff (px, py) (sx, sy)
    let r = distance (px, py) (sx, sy)
    let acceleration = (accelerationValue r) *** (ort (px, py) (sx, sy))
    setObjectSpeed ((vx , vy) +++ acceleration) ship

gameCycle :: IOGame () () () () ()
gameCycle = do
  let ships = getObjectManagerObjects spaceships1 ++ getObjectManagerObjects spaceships2
  forM_ ships handleGravity

  {-col1 <- objectLeftMapCollision ball
  col2 <- objectRightMapCollision ball
  when (col1 || col2) (reverseXSpeed ball)
  col3 <- objectTopMapCollision ball
  when col3 (reverseYSpeed ball)
  col4 <- objectBottomMapCollision ball
  when col4 $ do
    -- funExit
    reverseYSpeed ball

  bar <- findObject "bar" "barGroup"
  col5 <- objectsCollision ball bar
  let (_,vy) = getGameObjectSpeed ball
  when (and [col5, vy < 0])  (do reverseYSpeed ball)-}

