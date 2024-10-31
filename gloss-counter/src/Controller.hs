-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss.Interface.Pure.Game as G
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List


step :: Float -> GameState -> IO GameState
step secs gs@(GameState _ _ _ _ IsPaused) = return gs
step secs (GameState elapsedTime playerInfo asteroids bullets IsNotPaused) = return $ GameState (elapsedTime + secs) (updatePlayer playerInfo) (updateAsteroids asteroids) (updateBullets bullets) IsNotPaused
  where
    updatePlayer :: PlayerInfo -> PlayerInfo
    updatePlayer (PlayerInfo (x, y) angle True (False, False)) = movePlayer (PlayerInfo (x, y) angle True (False, False))
    updatePlayer (PlayerInfo (x, y) angle True (True, False)) = movePlayer (rotatePlayerLeft (PlayerInfo (x, y) angle True (True, False)))
    updatePlayer (PlayerInfo (x, y) angle True (False, True)) = movePlayer (rotatePlayerRight (PlayerInfo (x, y) angle True (False, True)))
    updatePlayer (PlayerInfo (x, y) angle True (True, True)) = movePlayer (PlayerInfo (x, y) angle True (True, True))
    updatePlayer (PlayerInfo (x, y) angle False (True, False)) = rotatePlayerLeft (PlayerInfo (x, y) angle False (True, False))
    updatePlayer (PlayerInfo (x, y) angle False (False, True)) = rotatePlayerRight (PlayerInfo (x, y) angle False (False, True))
    updatePlayer p = p

    updateAsteroids :: [Asteroid] -> [Asteroid]
    updateAsteroids = map moveAsteroid

    updateBullets :: [Bullet] -> [Bullet]
    updateBullets = map moveBullet



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

-- | Remove asteroids that are outside the screen
removeAsteroids :: [Asteroid] -> [Asteroid]
removeAsteroids = filter (\(Asteroid (x, y) _ _ _) -> x > -460 && x < 460 && y > -360 && y < 360)

removeBullets :: [Bullet] -> [Bullet]
removeBullets = filter (\(Bullet (x, y) _ _) -> x > -425 && x < 425 && y > -325 && y < 325)

isOutOfBounds :: (Float, Float) -> Bool
isOutOfBounds (x, y) = x > 425 || x < -425 || y > 325 || y < -325


--Generalisation of hitbox making for both the enemies and the players
class CreateHitBox a where
    mkHitBox :: a -> HitBox

--Makes hitbox for an enemy
instance CreateHitBox Asteroid where
    mkHitBox (Asteroid (x,y) _ _ SmallAsteroid) = [(x-20, y+20), (x+20, y-20)]
    mkHitBox (Asteroid (x,y) _ _ MediumAsteroid) = [(x-40, y+40), (x+40, y-40)]
    mkHitBox (Asteroid (x,y) _ _ LargeAsteroid) = [(x-60, y+60), (x+60, y-60)]
--Makes hitbox for a player
instance CreateHitBox PlayerInfo where
    mkHitBox(PlayerInfo (x, y) _ _ _) = [(x+50/3,y+25),(x+50/3,y-25),(x-(100/3),y)]

bulletCollisions :: [Bullet] -> [Asteroid] -> [Bullet]
bulletCollisions [] _ = []
bulletCollisions (b:bs) as | checkBulletHit b as = updateHitBullet b : bulletCollisions bs as
                           | otherwise           = b : bulletCollisions bs as

asteroidCollisions :: [Asteroid] -> [Bullet] -> [Asteroid]
asteroidCollisions [] _ = []
asteroidCollisions (a:as) bs | checkAsteroidHit a bs = updateHitAsteroid a : asteroidCollisions as bs
                             | otherwise             = a : asteroidCollisions as bs

checkBulletHit :: Bullet -> [Asteroid] -> Bool
checkBulletHit b []                      = False
checkBulletHit b@(Bullet pos _ _) (a:as) = isCollision (mkHitBox a) pos || checkBulletHit b as

checkAsteroidHit :: Asteroid -> [Bullet] -> Bool
checkAsteroidHit a []                      = False
checkAsteroidHit a (b@(Bullet pos _ _):bs) = isCollision (mkHitBox a) pos || checkAsteroidHit a bs

updateHitAsteroid :: Asteroid -> Asteroid
updateHitAsteroid (Asteroid pos d s LargeAsteroid) = Asteroid pos d s MediumAsteroid
updateHitAsteroid (Asteroid pos d s MediumAsteroid) = Asteroid pos d s SmallAsteroid
updateHitAsteroid (Asteroid pos d s SmallAsteroid) = Asteroid pos d s Destroyed

updateHitBullet :: Bullet -> Bullet
updateHitBullet (Bullet pos d _) = Bullet pos d True

isCollision :: HitBox -> (Float, Float) -> Bool
isCollision [(x1, y1), (x2, y2)] (x, y) = y < y1 && y > y2 && x > x1 && x < x2

