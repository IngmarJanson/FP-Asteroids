
module Deletion where

import Model
import Data.List (delete)

deleteBullets :: [Bullet] -> [Bullet]
deleteBullets [] = []
deleteBullets (b:bs) | hasHit b     = deleteBullets bs
                     | otherwise    = b : deleteBullets bs
  where
    hasHit (Bullet _ _ True)       = True
    hasHit _                       = False

deleteAsteroids :: [Asteroid] -> [Asteroid]
deleteAsteroids [] = []
deleteAsteroids (a:as) | isDestroyed a = deleteAsteroids as
                       | otherwise = a : deleteAsteroids as
  where
    isDestroyed (Asteroid _ _ _ Destroyed 0) = True
    isDestroyed _                          = False       

removeBullets :: [Bullet] -> [Bullet]
removeBullets = filter (\(Bullet (x, y) _ _) -> x > -425 && x < 425 && y > -325 && y < 325)