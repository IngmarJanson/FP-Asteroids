
module Collision where

import Model
import Graphics.Gloss (play)

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
asteroidCollisions (a:as) bs | checkAsteroidHit a bs = updateHitAsteroid a ++ asteroidCollisions as bs
                             | otherwise             = a : asteroidCollisions as bs

checkBulletHit :: Bullet -> [Asteroid] -> Bool
checkBulletHit b []                      = False
checkBulletHit b@(Bullet pos _ _) (a:as) = isCollision (mkHitBox a) pos || checkBulletHit b as

checkAsteroidHit :: Asteroid -> [Bullet] -> Bool
checkAsteroidHit a []                      = False
checkAsteroidHit a (b@(Bullet pos _ _):bs) = isCollision (mkHitBox a) pos || checkAsteroidHit a bs

updateHitAsteroid :: Asteroid -> [Asteroid]
updateHitAsteroid (Asteroid (x, y) d s LargeAsteroid) = [Asteroid (x + 3, y + 3) (d + 90) s MediumAsteroid, Asteroid (x - 3, y - 3) (d - 90) s MediumAsteroid]
updateHitAsteroid (Asteroid (x, y) d s MediumAsteroid) = [Asteroid (x + 3, y + 3) (d + 90) s SmallAsteroid, Asteroid (x - 3, y - 3) (d - 90) s SmallAsteroid]
updateHitAsteroid (Asteroid pos d s SmallAsteroid) = [Asteroid pos d s Destroyed]

updateHitBullet :: Bullet -> Bullet
updateHitBullet (Bullet pos d _) = Bullet pos d True

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
isPointInSquare (px, py) [(sx1,sy1), (sx2,sy2)] =
    px >= sx1 && px <= sx2 && py >= sy1 && py <= sy2

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

playerCollisions :: PlayerInfo -> [Asteroid] -> PlayerInfo
playerCollisions p@(PlayerInfo pos _ _ _) as | any (doesTriangleIntersectSquare (mkHitBox p) . mkHitBox) as = PlayerInfo (0,0) 90 False (False, False)
                                             | otherwise = p
