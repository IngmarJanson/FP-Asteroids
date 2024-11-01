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
step secs gs@(GameState _ _ _ _ IsPaused _ _) = return gs
step secs gs@(GameState _ (PlayerInfo _ _ _ _ Dead) _ _ _ _ _) = return gs
step secs (GameState elapsedTime playerInfo asteroids bullets IsNotPaused seed counter) = return $ GameState (elapsedTime + secs) (updatePlayer playerInfo) (updateAsteroids asteroids bullets) (updateBullets bullets asteroids) IsNotPaused seed (counter + 1)
  where
    updatePlayer :: PlayerInfo -> PlayerInfo
    updatePlayer p = movePlayer $ rotatePlayer $ playerCollisions p asteroids

    updateAsteroids :: [Asteroid] -> [Bullet] -> [Asteroid]
    updateAsteroids as bs = map moveAsteroid (createAsteroid position direction speed (deleteAsteroids (asteroidCollisions as bs)))
      where
        position = createPosition seed
        direction = createDirection seed
        speed = createSpeed seed

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
movePlayer (PlayerInfo (x, y) angle True isTurning status) =
  let newX = x - (4 * cos (angle * (pi /180)))
      newY = y + (4 * sin (angle * (pi /180)))
      (newX', newY') = if newX > 425 then (-425, newY) else if newX < -425 then (425, newY) else (newX, newY)
      (newX'', newY'') = if newY > 325 then (newX', -325) else if newY < -325 then (newX', 325) else (newX', newY')
  in PlayerInfo (newX'', newY'') angle True isTurning status
movePlayer p = p

-- | Rotate the player
rotatePlayer :: PlayerInfo -> PlayerInfo
rotatePlayer (PlayerInfo (x, y) angle isMoving (True,False) status) = PlayerInfo (x, y) (angle - 6) isMoving (True,False) status
rotatePlayer (PlayerInfo (x, y) angle isMoving (False,True) status) = PlayerInfo (x, y) (angle + 6) isMoving (False,True) status
rotatePlayer p = p

-- | Get starting position of bullet
startingPositionBullet :: Bullet -> Bullet
startingPositionBullet (Bullet (x, y) angle hit) = Bullet (x - (25 * cos (angle * (pi /180))), y + (25 * sin (angle * (pi /180))) ) angle hit

-- | Move the bullet
moveBullet :: Bullet -> Bullet
moveBullet (Bullet (x, y) angle hit) = Bullet (x - (15 * cos (angle * (pi /180))), y + (15 * sin (angle * (pi /180))) ) angle hit

-- | Move the asteroid
moveAsteroid :: Asteroid -> Asteroid
moveAsteroid (Asteroid (x, y) angle speed variant) = 
  let newX = x - (speed * cos (angle * (pi /180)))
      newY = y + (speed * sin (angle * (pi /180)))
      (newX', newY') = if newX > 400 + variantNumber then (-400 - variantNumber, newY) else if newX < -400 - variantNumber then (400 + variantNumber, newY) else (newX, newY)
      (newX'', newY'') = if newY > 300 + variantNumber then (newX', -300 - variantNumber) else if newY < -300 - variantNumber then (newX', 300 - variantNumber) else (newX', newY')
  in Asteroid (newX'', newY'') angle speed variant 
    where
      variantNumber = case variant of
        SmallAsteroid -> 20
        MediumAsteroid -> 40
        LargeAsteroid -> 60
        Destroyed -> 0

createPosition :: Int -> (Float, Float)
createPosition n = (x, y)
  where
    x = fst (randomR (-400, 400) (mkStdGen n))
    y = fst (randomR (-300, 300) (mkStdGen n))

createDirection :: Int -> Float
createDirection n = fst (randomR (0, 360) (mkStdGen n))

createSpeed :: Int -> Float
createSpeed n = fst (randomR (0, 10) (mkStdGen n))

createAsteroid :: (Float, Float) -> Float -> Float -> [Asteroid] -> [Asteroid]
createAsteroid pos d speed as   | length as < 8 = Asteroid pos d speed LargeAsteroid : as
                                | otherwise = as