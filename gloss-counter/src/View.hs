{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
-- | This module defines how to turn
--   the game state into a picture
--asteroids
module View where

import Graphics.Gloss
import Model
import Controller
import Deletion
import Collision (playerCollisions)

class Animate a where
    animate :: a -> a

instance Animate PlayerInfo where
  animate (PlayerInfo (x, y) angle True _ _) = PlayerInfo (x, y) angle True (False, False) Alive
  animate p = p

class Draw a where
    draw :: Picture -> a -> Picture

instance Draw PlayerInfo where
  draw pic (PlayerInfo (x, y) angle _ _ _) = translate x y $ rotate angle pic

      
instance Draw Asteroid where
  draw pic (Asteroid (x, y) _ _ SmallAsteroid) = translate x y pic
  draw pic (Asteroid (x, y) _ _ MediumAsteroid) = translate x y (scale 2 2 pic)
  draw pic (Asteroid (x, y) _ _ LargeAsteroid) = translate x y (scale 3 3 pic)

instance Draw Bullet where
  draw pic (Bullet (x, y) _ _) = translate x y pic

view :: Picture -> Picture -> Picture -> GameState -> Picture
view playerCircle asteroidBlock bulletDot gs@(GameState _ playerInfo asteroids bullets _ _ _) = pictures 
  [
    draw playerCircle (playerInfo)
    , pictures $ map (draw asteroidBlock) asteroids
    , pictures $ map (draw bulletDot) (removeBullets bullets)
    , showPauseScreen gs
    , showGameOverScreen gs
  ]

pauseScreen :: Picture
pauseScreen = pictures [color white $ translate (-100) 0 $ scale 0.3 0.3 $ text "Paused", color white $ translate (-100) (-50) $ scale 0.1 0.1 $ text "Press 'p' to unpause"]

gameOverScreen :: Picture
gameOverScreen  = pictures [color white $ translate (-100) 0 $ scale 0.3 0.3 $ text "Game Over", color white $ translate (-100) (-50) $ scale 0.1 0.1 $ text "Press 'r' to restart"]

showGameOverScreen :: GameState -> Picture
showGameOverScreen (GameState _ (PlayerInfo _ _ _ _ Dead)  _ _ _ _ _) = gameOverScreen
showGameOverScreen _ = blank

showPauseScreen :: GameState -> Picture
showPauseScreen (GameState _ _ _ _ IsPaused _ _) = pauseScreen
showPauseScreen _ = blank