module SpaceGame where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)
import Geometry
import Physics

data GameAttribute = Score Int

width = 400
height = 400
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble
planetCenterX = w / 2
planetCenterY = h / 2

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return

main :: IO ()
main = do
  let winConfig = ((100,80),(width,height),"Space game")
      gameMap = colorMap 0 0 0 w h
      planet    = objectGroup "planetGroup" [createPlanet]
      spaceship = objectGroup "spaceshipGroup" [createSpaceship]
      {-input = [
        (SpecialKey KeyRight, StillDown, moveBarToRight)
        ,(SpecialKey KeyLeft,  StillDown, moveBarToLeft)
        ,(Char 'q',            Press,     \_ _ -> funExit)
        ]-}
  funInit winConfig gameMap [planet, spaceship] () (()) [] (return()) (Timer 30) []

createPlanet :: GameObject ()
createPlanet =
  let planetPic = Basic (Circle 20.0 1.0 0.0 0.0 Filled)
  in object "planet" planetPic False (w/2,h/2) (0,0) ()

createSpaceship :: GameObject ()
createSpaceship =
  let spaceshipPic = Basic (Circle 2.0 0.0 1.0 0.0 Filled)
  in object "spaceship" spaceshipPic False (w/2 + 5.0,h/2) (0,1) ()

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

gameCycle :: IOGame () () () () ()
gameCycle = do
  spaceship <- findObject "spaceship" "spaceshipGroup"
  (vx, vy) <- getGameObjectSpeed spaceship
  (sx, sy) <- getObjectPosition spaceship
  (px, py) <- getObjectPosition planet
  let (dx, dy) = diff (px, py) (sx, sy)
  let r = distance (px, py) (sx, sy)
  let acceleration = (accelerationValue r) *** (ort (px, py) (sx, sy))
  setObjectSpeed ((vx, vy) +++ acceleration) spaceship

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

