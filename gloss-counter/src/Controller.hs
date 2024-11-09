{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
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
import Score

step :: Float -> GameState -> IO GameState
step secs gs@(GameState _ _ _ _ IsPaused _ _ _) = return gs
step secs gs@(GameState _ (PlayerInfo _ _ _ _ Dead _ _) _ _ _ _ _ _) = return gs
step secs (GameState elapsedTime playerInfo asteroids bullets IsNotPaused seed counter scores) = return $ GameState (elapsedTime + secs) (updatePlayer playerInfo) (updateAsteroids asteroids bullets) (updateBullets bullets asteroids) IsNotPaused seed (counter + 1) scores
  where
    updatePlayer :: PlayerInfo -> PlayerInfo
    updatePlayer p = movePlayer $ rotatePlayer $ updateScore asteroids $ playerCollisions p asteroids

    updateAsteroids :: [Asteroid] -> [Bullet] -> [Asteroid]
    updateAsteroids as bs = map moveAsteroid $ setAlienDirection (playerPosition playerInfo) $ createAsteroid counter positions directions speeds maxNumAsteroids variant $ deleteAsteroids $ asteroidCollisions as bs
      where
        positions = createPositions seed counter (playerPosition playerInfo)
        directions = createDirections seed counter
        speeds = createSpeeds speedRange seed counter
        speedRange = getSpeedRange counter
        maxNumAsteroids = getMaxNumAsteroids counter
        variant = createVariants variantRange seed counter
        variantRange = getVariantRange counter

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

input (EventKey (SpecialKey KeySpace) Down _ (x, y)) gstate = return $ gstate { bullets = startingPositionBullet (Bullet (playerPosition (playerInfo gstate)) (playerDirection (playerInfo gstate)) False) : bullets gstate }


-- left click mouse button to shoot
input (EventKey (MouseButton LeftButton) Down _ (x, y)) gstate = return $ gstate { bullets = startingPositionBullet (Bullet (playerPosition (playerInfo gstate)) (playerDirection (playerInfo gstate)) False) : bullets gstate }

-- right click turns player to mouse position
input (EventKey (MouseButton RightButton) Down _ _) gstate = return $ gstate { playerInfo = (playerInfo gstate) { isMoving = True } }
input (EventKey (MouseButton RightButton) Up _ _) gstate = return $ gstate { playerInfo = (playerInfo gstate) { isMoving = False } }

input (EventKey (MouseButton MiddleButton) Down _ _) gstate = return $ gstate { playerInfo = (playerInfo gstate) { isTurning = (True, snd $ isTurning (playerInfo gstate)) } }
input (EventKey (MouseButton MiddleButton) Up _ _) gstate = return $ gstate { playerInfo = (playerInfo gstate) { isTurning = (False, snd $ isTurning (playerInfo gstate)) } }


input (EventKey (Char 'r') Down _ _) gs@(GameState _ player@(PlayerInfo _ _ _ _ dead _ _) _ _ _ _ _ scores) = do
  seed <- randomIO
  return $ initialState seed scores

-- right enter key to save game and update highscores
input (EventKey (SpecialKey KeyEnter) Down _ _) gs@(GameState _ player@(PlayerInfo _ _ _ _ dead _ _) _ _ _ _ _ scores) = do
  writeFile "highscores.txt" (updateHighScores scores player)
  return gs

input _ gstate = return gstate

-- | Move the player
movePlayer :: PlayerInfo -> PlayerInfo
movePlayer (PlayerInfo (x, y) angle True isTurning status score name) =
  let newX = x - (4 * cos (angle * (pi /180)))
      newY = y + (4 * sin (angle * (pi /180)))
      (newX', newY') = if newX > 425 then (-425, newY) else if newX < -425 then (425, newY) else (newX, newY)
      (newX'', newY'') = if newY > 325 then (newX', -325) else if newY < -325 then (newX', 325) else (newX', newY')
  in PlayerInfo (newX'', newY'') angle True isTurning status score name
movePlayer p = p

-- | Rotate the player
rotatePlayer :: PlayerInfo -> PlayerInfo
rotatePlayer (PlayerInfo (x, y) angle isMoving (True,False) status score name) = PlayerInfo (x, y) (angle - 6) isMoving (True,False) status score name
rotatePlayer (PlayerInfo (x, y) angle isMoving (False,True) status score name) = PlayerInfo (x, y) (angle + 6) isMoving (False,True) status score name
rotatePlayer p = p

updateScore :: [Asteroid] -> PlayerInfo -> PlayerInfo
updateScore [] p = p
updateScore ((Asteroid _ _ _ _ plusScore):as) (PlayerInfo p angle m t s score name) = updateScore as (PlayerInfo p angle m t s (score + plusScore) name)

-- | Get starting position of bullet
startingPositionBullet :: Bullet -> Bullet
startingPositionBullet (Bullet (x, y) angle hit) = Bullet (x - (25 * cos (angle * (pi /180))), y + (25 * sin (angle * (pi /180))) ) angle hit

-- | Move the bullet
moveBullet :: Bullet -> Bullet
moveBullet (Bullet (x, y) angle hit) = Bullet (x - (15 * cos (angle * (pi /180))), y + (15 * sin (angle * (pi /180))) ) angle hit

-- | Move the asteroid
moveAsteroid :: Asteroid -> Asteroid
moveAsteroid (Asteroid (x, y) angle speed variant score) =
  let newX = x - (speed * cos (angle * (pi /180)))
      newY = y + (speed * sin (angle * (pi /180)))
      (newX', newY') = if newX > 400 + variantNumber then (-400 - variantNumber, newY) else if newX < -400 - variantNumber then (400 + variantNumber, newY) else (newX, newY)
      (newX'', newY'') = if newY > 300 + variantNumber then (newX', -300 - variantNumber) else if newY < -300 - variantNumber then (newX', 300 - variantNumber) else (newX', newY')
  in Asteroid (newX'', newY'') angle speed variant score
    where
      variantNumber = case variant of
        SmallAsteroid -> 20
        MediumAsteroid -> 40
        LargeAsteroid -> 60
        Alien -> 20
        Destroyed -> 0

createPositions :: Int -> Int -> (Float, Float) -> [(Float, Float)]
createPositions n counter (pX, pY) = if length positionsSet == 25 then positionsSet else []
  where
    positionsSet = take 25 (filter f [(x, y) | x <- xSet, y <-ySet])
    xSet = getRandomNumbers 500 (-400) 400 n counter
    ySet = getRandomNumbers 500 (-300) 300 n counter
    f (x, y) = not (x > (pX - 200) && x < (pX + 200) && y > (pY - 200) && y < (pY + 200))

createDirections :: Int -> Int -> [Float]
createDirections = getRandomNumbers 25 0 360

createSpeeds :: (Float, Float) -> Int -> Int -> [Float]
createSpeeds (lowEnd, highEnd)= getRandomNumbers 25 lowEnd highEnd

getSpeedRange :: Int -> (Float, Float)
getSpeedRange counter | counter < 1800  = (2, 4)
                      | counter < 3600  = (2, 5)
                      | counter < 5400  = (3, 5)
                      | counter < 7200  = (3, 6)
                      | counter < 9000  = (4, 6)
                      | counter < 10800 = (4, 7)
                      | counter < 12600 = (5, 7)
                      | counter < 14400 = (5, 8)
                      | counter < 16200 = (6, 8)
                      | counter < 18000 = (6, 9)
                      | otherwise       = (7, 10)

getMaxNumAsteroids :: Int -> Int
getMaxNumAsteroids counter | counter < 1800  = 5
                           | counter < 3600  = 6
                           | counter < 7200  = 7
                           | counter < 10800 = 8
                           | counter < 14400 = 9
                           | otherwise       = 10

createVariants :: (Float, Float) -> Int -> Int -> [Variant]
createVariants (lowEnd, highEnd) n counter = map (toEnum . round) $ getRandomNumbers 25 lowEnd highEnd n counter

getVariantRange :: Int -> (Float, Float)
getVariantRange counter | counter < 900  = (1, 3)
                        | otherwise      = (1, 4)

getRandomNumbers :: Int -> Float -> Float -> Int -> Int -> [Float]
getRandomNumbers num lowEnd highEnd n counter = take num (unfoldr (Just .uniformR (lowEnd, highEnd)) (mkStdGen (n + counter)))

createAsteroid :: Int -> [(Float, Float)] -> [Float] -> [Float] -> Int -> [Variant] -> [Asteroid] -> [Asteroid]
createAsteroid n pos d speeds max vars as | null pos = as
                                    | n >= length pos = createAsteroid (n - length pos) pos d speeds max vars as
                                    | length as <= max = Asteroid (pos !! n) (d !! n) (speeds !! n) (vars !! n) 0 : as
                                    | otherwise = as

setAlienDirection :: (Float, Float) -> [Asteroid] -> [Asteroid]
setAlienDirection _ [] = []
setAlienDirection (x1, y1) ((Asteroid (x2, y2) d speed Alien s):as) = Asteroid (x2, y2) (((atan2 (x1 - x2) (y1 - y2) * 180) / pi) + 90) speed Alien s : setAlienDirection (x1, y1) as
setAlienDirection p (a:as) = a : setAlienDirection p as