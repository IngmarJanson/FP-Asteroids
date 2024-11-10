
module Deletion where

import Model
import Data.List (delete)

--Removes a bullet from the bullets list if it's been hit
deleteBullets :: [Bullet] -> [Bullet]
deleteBullets = filter (not . hasHit)
  where
    hasHit (Bullet _ _ True) = True
    hasHit _                 = False

--Removes an asteroid from the asteroids list if it's been destroyed and the explosion animation has ended (so at frame 8)
deleteAsteroids :: [Asteroid] -> [Asteroid]
deleteAsteroids = filter (not . isDestroyed)
  where
    isDestroyed (Asteroid _ _ _ Destroyed 0 8) = True
    isDestroyed _                              = False       

--Removes a bullet from the bullets list if it's outside the screen
removeBullets :: [Bullet] -> [Bullet]
removeBullets = filter (\(Bullet (x, y) _ _) -> x > -425 && x < 425 && y > -325 && y < 325)