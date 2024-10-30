-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss.Interface.Pure.Game as G

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

step :: Float -> GameState -> IO GameState
step secs (GameState elapsedTime playerInfo asteroids) = return $ GameState (elapsedTime + secs) (updatePlayer playerInfo) (updateAsteroids asteroids)
  where
    updatePlayer :: PlayerInfo -> PlayerInfo
    updatePlayer (PlayerInfo (x, y) (vx, vy) True (False, False)) = movePlayer (PlayerInfo (x, y) (vx, vy) True (False, False))
    updatePlayer (PlayerInfo (x, y) (vx, vy) True (True, False)) = movePlayer(rotatePlayerLeft (PlayerInfo (x, y) (vx, vy) True (True, False)))
    updatePlayer (PlayerInfo (x, y) (vx, vy) True (False, True)) = movePlayer(rotatePlayerRight (PlayerInfo (x, y) (vx, vy) True (False, True)))
    updatePlayer (PlayerInfo (x, y) (vx, vy) False (True, False)) = rotatePlayerLeft (PlayerInfo (x, y) (vx, vy) False (True, False))
    updatePlayer (PlayerInfo (x, y) (vx, vy) False (False, True)) = rotatePlayerRight (PlayerInfo (x, y) (vx, vy) False (False, True))
    updatePlayer p = p

    updateAsteroids :: [Asteroid] -> [Asteroid]
    updateAsteroids a = a



input :: Event -> GameState -> IO GameState
input (EventKey (SpecialKey KeyUp) Down _ _) gstate = return $ gstate { playerInfo = (playerInfo gstate) { isMoving = True } }
input (EventKey (SpecialKey KeyUp) Up _ _) gstate = return $ gstate { playerInfo = (playerInfo gstate) { isMoving = False } }
input (EventKey (SpecialKey KeyLeft) Down _ _) gstate = return $ gstate { playerInfo = (playerInfo gstate) { isTurning = (True, False) } }
input (EventKey (SpecialKey KeyLeft) Up _ _) gstate = return $ gstate { playerInfo = (playerInfo gstate) { isTurning = (False, False) } }
input (EventKey (SpecialKey KeyRight) Down _ _) gstate = return $ gstate { playerInfo = (playerInfo gstate) { isTurning = (False, True) } }
input (EventKey (SpecialKey KeyRight) Up _ _) gstate = return $ gstate { playerInfo = (playerInfo gstate) { isTurning = (False, False) } }
input _ gstate = return gstate


-- | Move the player
movePlayer :: PlayerInfo -> PlayerInfo
movePlayer (PlayerInfo (x, y) (vx, vy) isMoving isTurning) = 
  let newX = x + (vx * 3)
      newY = y + (vy * 3)
      (newX', newY') = if newX > 425 then (-425, newY) else if newX < -425 then (425, newY) else (newX, newY)
      (newX'', newY'') = if newY > 325 then (newX', -325) else if newY < -325 then (newX', 325) else (newX', newY')

  in PlayerInfo (newX'', newY'') (vx, vy) isMoving isTurning
-- | Rotate the player
rotatePlayerLeft :: PlayerInfo -> PlayerInfo
rotatePlayerLeft (PlayerInfo (x, y) (vx, vy) isMoving isTurning) = 
  let angle = atan2 vy vx + pi / 18 -- rotate by 10 degrees (pi/18 radians)
      newVx = cos angle
      newVy = sin angle
  in PlayerInfo (x, y) (newVx, newVy) isMoving isTurning

-- | Rotate the player
rotatePlayerRight :: PlayerInfo -> PlayerInfo
rotatePlayerRight (PlayerInfo (x, y) (vx, vy) isMoving isTurning) = 
  let angle = atan2 vy vx - pi / 18 -- rotate by 10 degrees (pi/18 radians)
      newVx = cos angle
      newVy = sin angle
  in PlayerInfo (x, y) (newVx, newVy) isMoving isTurning


