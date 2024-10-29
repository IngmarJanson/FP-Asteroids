-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss.Interface.Pure.Game as G

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

step :: Float -> GameState -> IO GameState
step secs (GameState elapsedTime playerInfo) = return $ GameState (elapsedTime + secs) (updatePlayer playerInfo)
  where
    updatePlayer :: PlayerInfo -> PlayerInfo
    updatePlayer (PlayerInfo (x, y) (vx, vy) True (False, False)) = movePlayer (PlayerInfo (x, y) (vx, vy) True (False, False))
    updatePlayer (PlayerInfo (x, y) (vx, vy) True (True, False)) = movePlayer(rotatePlayerLeft (PlayerInfo (x, y) (vx, vy) True (True, False)))
    updatePlayer (PlayerInfo (x, y) (vx, vy) True (False, True)) = movePlayer(rotatePlayerRight (PlayerInfo (x, y) (vx, vy) True (False, True)))
    updatePlayer (PlayerInfo (x, y) (vx, vy) False (True, False)) = rotatePlayerLeft (PlayerInfo (x, y) (vx, vy) False (True, False))
    updatePlayer (PlayerInfo (x, y) (vx, vy) False (False, True)) = rotatePlayerRight (PlayerInfo (x, y) (vx, vy) False (False, True))
    updatePlayer p = p




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
movePlayer (PlayerInfo (x, y) (vx, vy) isMoving isTurning) = PlayerInfo (x + (vx * 3), y + (vy * 3)) (vx, vy) isMoving isTurning

-- | Rotate the player
rotatePlayerLeft :: PlayerInfo -> PlayerInfo
rotatePlayerLeft (PlayerInfo (x, y) (vx, vy) isMoving isTurning) = 
  let angle = atan2 vy vx + pi / 18 -- rotate by 10 degrees (pi/18 radians)
      speed = sqrt (vx * vx + vy * vy)
      newVx = speed * cos angle
      newVy = speed * sin angle
  in PlayerInfo (x, y) (newVx, newVy) isMoving isTurning
rotatePlayerLeft p = p

-- | Rotate the player
rotatePlayerRight :: PlayerInfo -> PlayerInfo
rotatePlayerRight (PlayerInfo (x, y) (vx, vy) isMoving isTurning) = 
  let angle = atan2 vy vx - pi / 18 -- rotate by 10 degrees (pi/18 radians)
      speed = sqrt (vx * vx + vy * vy)
      newVx = speed * cos angle
      newVy = speed * sin angle
  in PlayerInfo (x, y) (newVx, newVy) isMoving isTurning
rotatePlayerRight p = p

