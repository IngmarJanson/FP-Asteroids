-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss.Interface.Pure.Game as G
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List
import Deletion
import Collision

step :: Float -> GameState -> IO GameState
step secs gs@(GameState _ _ _ _ IsPaused) = return gs
step secs (GameState elapsedTime playerInfo asteroids bullets IsNotPaused) = return $ GameState (elapsedTime + secs) (updatePlayer playerInfo) (updateAsteroids asteroids bullets) (updateBullets bullets asteroids) IsNotPaused
  where
    updatePlayer :: PlayerInfo -> PlayerInfo
    updatePlayer (PlayerInfo (x, y) angle True (False, False)) = movePlayer (PlayerInfo (x, y) angle True (False, False))
    updatePlayer (PlayerInfo (x, y) angle True (True, False)) = movePlayer (rotatePlayerLeft (PlayerInfo (x, y) angle True (True, False)))
    updatePlayer (PlayerInfo (x, y) angle True (False, True)) = movePlayer (rotatePlayerRight (PlayerInfo (x, y) angle True (False, True)))
    updatePlayer (PlayerInfo (x, y) angle True (True, True)) = movePlayer (PlayerInfo (x, y) angle True (True, True))
    updatePlayer (PlayerInfo (x, y) angle False (True, False)) = rotatePlayerLeft (PlayerInfo (x, y) angle False (True, False))
    updatePlayer (PlayerInfo (x, y) angle False (False, True)) = rotatePlayerRight (PlayerInfo (x, y) angle False (False, True))
    updatePlayer p = p

    updateAsteroids :: [Asteroid] -> [Bullet] -> [Asteroid]
    updateAsteroids as bs = map moveAsteroid (deleteAsteroids (asteroidCollisions as bs))

    updateBullets :: [Bullet] -> [Asteroid] -> [Bullet]
    updateBullets bs as = map moveBullet (deleteBullets (bulletCollisions bs as))


input :: Event -> GameState -> IO GameState
input (EventKey (Char 'p') Down _ _) gstate = return $ gstate { pauseState = if pauseState gstate == IsPaused then IsNotPaused else IsPaused }

input (EventKey (SpecialKey KeyUp) Down _ _) gstate = return $ gstate { playerInfo = (playerInfo gstate) { isMoving = True } }
input (EventKey (SpecialKey KeyUp) Up _ _) gstate = return $ gstate { playerInfo = (playerInfo gstate) { isMoving = False } }
input (EventKey (SpecialKey KeyLeft) Down _ _) gstate = return $ gstate { playerInfo = (playerInfo gstate) { isTurning = (True, snd $ isTurning (playerInfo gstate)) } }
input (EventKey (SpecialKey KeyLeft) Up _ _) gstate = return $ gstate { playerInfo = (playerInfo gstate) { isTurning = (False, snd $ isTurning (playerInfo gstate)) } }
input (EventKey (SpecialKey KeyRight) Down _ _) gstate = return $ gstate { playerInfo = (playerInfo gstate) { isTurning = (fst $ isTurning (playerInfo gstate), True) } }
input (EventKey (SpecialKey KeyRight) Up _ _) gstate = return $ gstate { playerInfo = (playerInfo gstate) { isTurning = (fst $ isTurning (playerInfo gstate), False) } }

input (EventKey (SpecialKey KeySpace) Down _ _) gstate = return $ gstate { bullets = startingPositionBullet (Bullet (playerPosition (playerInfo gstate)) (playerDirection (playerInfo gstate)) False) : bullets gstate }

input _ gstate = return gstate

-- | Move the player
movePlayer :: PlayerInfo -> PlayerInfo
movePlayer (PlayerInfo (x, y) angle isMoving isTurning) =
  let newX = x - (4 * cos (angle * (pi /180)))
      newY = y + (4 * sin (angle * (pi /180)))
      (newX', newY') = if newX > 425 then (-425, newY) else if newX < -425 then (425, newY) else (newX, newY)
      (newX'', newY'') = if newY > 325 then (newX', -325) else if newY < -325 then (newX', 325) else (newX', newY')
  in PlayerInfo (newX'', newY'') angle isMoving isTurning

-- | Rotate the player
rotatePlayerLeft :: PlayerInfo -> PlayerInfo
rotatePlayerLeft (PlayerInfo (x, y) angle isMoving isTurning) = PlayerInfo (x, y) (angle - 6) isMoving isTurning

-- | Rotate the player
rotatePlayerRight :: PlayerInfo -> PlayerInfo
rotatePlayerRight (PlayerInfo (x, y) angle isMoving isTurning) = PlayerInfo (x, y) (angle + 6) isMoving isTurning

-- | Get starting position of bullet
startingPositionBullet :: Bullet -> Bullet
startingPositionBullet (Bullet (x, y) angle hit) = Bullet (x - (25 * cos (angle * (pi /180))), y + (25 * sin (angle * (pi /180))) ) angle hit

-- | Move the bullet
moveBullet :: Bullet -> Bullet
moveBullet (Bullet (x, y) angle hit) = Bullet (x - (15 * cos (angle * (pi /180))), y + (15 * sin (angle * (pi /180))) ) angle hit

-- | Move the asteroid
moveAsteroid :: Asteroid -> Asteroid
moveAsteroid (Asteroid (x, y) angle speed variant) = Asteroid (x - (speed * cos (angle * (pi /180))), y + (speed * sin (angle * (pi /180))) ) angle speed variant



