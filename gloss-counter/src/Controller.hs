-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss.Interface.Pure.Game as G

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- create a function that updates the state of the game
-- | Update the game state
step :: Float -> GameState -> IO GameState
step secs gstate = return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- create a function that handles user input
input :: Event -> GameState -> IO GameState
input (EventKey (SpecialKey KeyUp) G.Down _ _) gstate = return $ gstate { playerInfo = movePlayer(playerInfo gstate) { isMoving = True } }
input (EventKey (SpecialKey KeyUp) G.Up _ _) gstate = return $ gstate { playerInfo = (playerInfo gstate) { isMoving = False } }
input (EventKey (SpecialKey KeyLeft) G.Down _ _) gstate = return $ gstate { playerInfo = rotatePlayerLeft(playerInfo gstate) { isMoving = True } }
input (EventKey (SpecialKey KeyLeft) G.Up _ _) gstate = return $ gstate { playerInfo = (playerInfo gstate) { isMoving = False } }
input (EventKey (SpecialKey KeyRight) G.Down _ _) gstate = return $ gstate { playerInfo = rotatePlayerRight(playerInfo gstate) { isMoving = True } }
input (EventKey (SpecialKey KeyRight) G.Up _ _) gstate = return $ gstate { playerInfo = (playerInfo gstate) { isMoving = False } }
input _ gstate = return gstate

-- | Move the player
movePlayer :: PlayerInfo -> PlayerInfo
movePlayer (PlayerInfo (x, y) (vx, vy) _) = PlayerInfo (x + vx, y + vy) (vx, vy) True

-- | Rotate the player
rotatePlayerLeft :: PlayerInfo -> PlayerInfo
rotatePlayerLeft (PlayerInfo (x, y) (vx, vy) _) = 
  let angle = atan2 vy vx + pi / 18 -- rotate by 10 degrees (pi/18 radians)
      speed = sqrt (vx * vx + vy * vy)
      newVx = speed * cos angle
      newVy = speed * sin angle
  in PlayerInfo (x, y) (newVx, newVy) True

-- | Rotate the player
rotatePlayerRight :: PlayerInfo -> PlayerInfo
rotatePlayerRight (PlayerInfo (x, y) (vx, vy) _) = 
  let angle = atan2 vy vx - pi / 18 -- rotate by 10 degrees (pi/18 radians)
      speed = sqrt (vx * vx + vy * vy)
      newVx = speed * cos angle
      newVy = speed * sin angle
  in PlayerInfo (x, y) (newVx, newVy) True

