
module Collision where

import Model
import Graphics.Gloss (play)

--Generalisation of hitbox making for both the enemies and the players
class CreateHitBox a where
    mkHitBox :: a -> HitBox

--Makes hitbox for an asteroid
instance CreateHitBox Asteroid where
    mkHitBox (Asteroid (x,y) _ _ SmallAsteroid _ _)  = [(x-15, y+15), (x+15, y-15)]
    mkHitBox (Asteroid (x,y) _ _ MediumAsteroid _ _) = [(x-35, y+35), (x+35, y-35)]
    mkHitBox (Asteroid (x,y) _ _ LargeAsteroid _ _)  = [(x-55, y+55), (x+55, y-55)]
    mkHitBox (Asteroid (x,y) _ _ AlienAsteroid _ _)          = [(x-15, y+15), (x+15, y-15)]
    mkHitBox (Asteroid (x, y) _ _ Destroyed _ _)     = [(x, y), (x, y)]

--Makes hitbox for a player
instance CreateHitBox PlayerInfo where
    mkHitBox(PlayerInfo (x, y) _ _ _ _ _ _) = [(x+50/3,y+25),(x+50/3,y-25),(x-(100/3),y)]

--Checks for all bullets if they hit any asteroid and then updates the bullets 'hit' that do to True
bulletCollisions :: [Bullet] -> [Asteroid] -> [Bullet]
bulletCollisions [] _                            = []
bulletCollisions (b:bs) as | checkBulletHit b as = b{hit=True} : bulletCollisions bs as
                           | otherwise           = b : bulletCollisions bs as

--Checks if a single bullet hits any of the asteroids
checkBulletHit :: Bullet -> [Asteroid] -> Bool
checkBulletHit (Bullet pos _ _) = any (\a -> isCollision (mkHitBox a) pos)

--Checks for all asteroids if they have been hit by any bullet and then updates the hit asteroid, also sets the asteroidScoreToGive to zero for asteroids that haven't been hit
asteroidCollisions :: [Asteroid] -> [Bullet] -> [Asteroid]
asteroidCollisions [] _                              = []
asteroidCollisions (a:as) bs | checkAsteroidHit a bs = updateHitAsteroid a ++ asteroidCollisions as bs
                             | otherwise             = a{asteroidScoreToGive=0} : asteroidCollisions as bs

--Checks if a single asteroid has been hit by any bullet
checkAsteroidHit :: Asteroid -> [Bullet] -> Bool
checkAsteroidHit a = any (\(Bullet pos _ _) -> isCollision (mkHitBox a) pos)

--An asteroid that is destroyed falls apart in a destroyed asteroid, which is used for the explosion animation and to give out points to the player.
--The amount of points given is based on the asteroid that is destroyed (Alien: 50, LargeAsteroid: 30, MediumAsteroid: 20, SmallAsteroid: 10)
--The large and medium asteroids get destroyed and spawn two new smaller asteroids with a perpendicular direction to the original asteroid
updateHitAsteroid :: Asteroid -> [Asteroid]
updateHitAsteroid (Asteroid (x, y) d s LargeAsteroid _ _)  = [Asteroid (x, y) d 0 Destroyed 30 0, Asteroid (x + 3, y + 3) (d + 90) s MediumAsteroid 0 0, Asteroid (x - 3, y - 3) (d - 90) s MediumAsteroid 0 0]
updateHitAsteroid (Asteroid (x, y) d s MediumAsteroid _ _) = [Asteroid (x, y) d 0 Destroyed 20 0, Asteroid (x + 3, y + 3) (d + 90) s SmallAsteroid 0 0, Asteroid (x - 3, y - 3) (d - 90) s SmallAsteroid 0 0]
updateHitAsteroid (Asteroid pos d s SmallAsteroid _ _)     = [Asteroid pos d 0 Destroyed 10 0]
updateHitAsteroid (Asteroid pos d s AlienAsteroid _ _)     = [Asteroid pos d 0 Destroyed 50 0]
updateHitAsteroid a@(Asteroid _ _ _ Destroyed _ _)         = [a]

--Checks if a point hits/is inside a hitbox
isCollision :: HitBox -> (Float, Float) -> Bool
isCollision [(x1, y1), (x2, y2)] (x, y) = y < y1 && y > y2 && x > x1 && x < x2

-- Function to check if two line segments (p1, p2) and (p3, p4) intersect
doLinesIntersect :: (Float, Float) -> (Float,Float) -> (Float,Float) -> (Float,Float) -> Bool
doLinesIntersect (x1, y1) (x2, y2) (x3, y3) (x4, y4) =
    let d = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
        t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / d
        u = -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / d
    in d /= 0 && t >= 0 && t <= 1 && u >= 0 && u <= 1

-- Function to check if a point is inside a square
isPointInSquare :: (Float,Float) -> HitBox -> Bool
isPointInSquare (px, py) [(sx1,sy1), (sx2,sy2)] = px >= sx1 && px <= sx2 && py >= sy1 && py <= sy2

-- Function to check if a point is inside a triangle using barycentric coordinates
isPointInTriangle :: (Float,Float) -> HitBox -> Bool
isPointInTriangle (x,y) [(x1,y1), (x2,y2), (x3,y3)] =
    let
        denominator = ((y2 - y3)*(x1 - x3) + (x3 - x2)*(y1 - y3))
        a = ((y2 - y3)*(x - x3) + (x3 - x2)*(y - y3)) / denominator
        b = ((y3 - y1)*(x - x3) + (x1 - x3)*(y - y3)) / denominator
        c = 1 - a - b
    in a >= 0 && a <= 1 && b >= 0 && b <= 1 && c >= 0 && c <= 1

-- Function to check if a triangle intersects with a square
doesTriangleIntersectSquare :: HitBox -> HitBox -> Bool
doesTriangleIntersectSquare [(x1,y1), (x2,y2), (x3,y3)] square@[(x1Square,y1Square), (x2Square,y2Square)] =
    let squarePoints = [
                        (x1Square, y1Square),
                        (x2Square, y1Square),
                        (x2Square, y2Square),
                        (x1Square, y2Square)
                       ]
        squareEdges = [(squarePoints !! i, squarePoints !! ((i + 1) `mod` 4)) | i <- [0..3]]
        triangleEdges = [((x1,y1), (x2,y2)), ((x2,y2), (x3,y3)), ((x3,y3), (x1,y1))]
        -- Check if any triangle edge intersects any square edge
        edgeIntersection = any (\((x1,y1), (x2,y2)) -> any (uncurry (doLinesIntersect (x1,y1) (x2,y2))) squareEdges) triangleEdges
        -- Check if any triangle vertex is inside the square
        vertexInSquare = any (`isPointInSquare` square) [(x1,y1), (x2,y2), (x3,y3)]
        -- Check if any square vertex is inside the triangle
        vertexInTriangle = any (`isPointInTriangle` [(x1,y1), (x2,y2), (x3,y3)])  squarePoints
    in edgeIntersection || vertexInSquare || vertexInTriangle

--Checks if a player is hitting any asteroid and if they do updates the player accordingly
playerCollisions :: PlayerInfo -> [Asteroid] -> PlayerInfo
playerCollisions p@(PlayerInfo pos _ _ _ Three score name) as = if any (doesTriangleIntersectSquare (mkHitBox p) . mkHitBox) as then PlayerInfo (0,0) 90 NotMoving (False, False) Two score name else p
playerCollisions p@(PlayerInfo pos _ _ _ Two score name) as   = if any (doesTriangleIntersectSquare (mkHitBox p) . mkHitBox) as then PlayerInfo (0,0) 90 NotMoving (False, False) One score name else p
playerCollisions p@(PlayerInfo pos _ _ _ One score name) as   = if any (doesTriangleIntersectSquare (mkHitBox p) . mkHitBox) as then PlayerInfo (0,0) 90 NotMoving (False, False) Dead score name else p
