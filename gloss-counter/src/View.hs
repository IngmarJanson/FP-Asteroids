-- | This module defines how to turn
--   the game state into a picture
--asteroids
module View where

import Graphics.Gloss
import Model
class Animate a where 
    animate :: a -> a 

instance Animate PlayerInfo where
  animate (PlayerInfo (x, y) (vx, vy) True _) = PlayerInfo (x + vx, y + vy) (vx, vy) True (False, False)
  animate p = p

class Draw a where
    draw :: Picture -> a -> Picture

instance Draw PlayerInfo where
  draw pic (PlayerInfo (x, y) _ _ _) = translate x y pic


view :: Picture -> GameState -> IO Picture
view playerCircle gstate = return $ draw playerCircle (playerInfo gstate)





