-- | This module contains the data types
--   which represent the state of the game
module Model where
import GHC.Integer (smallInteger)

type Left = Bool
type Right = Bool

data Variant = Destroyed | SmallAsteroid | MediumAsteroid | LargeAsteroid

data PlayerStatus = Alive | Dead
--Describing the hit box as a list of points
type HitBox = [(Float, Float)]

data Paused = IsPaused | IsNotPaused
    deriving (Show, Eq)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1

data GameState = GameState {
                   elapsedTime :: Float
                  , playerInfo :: PlayerInfo
                  , asteroids :: [Asteroid]
                  , bullets :: [Bullet]
                  , pauseState :: Paused
                  , seed :: Int
                  , counter :: Int
}

data PlayerInfo = PlayerInfo {
                    playerPosition :: (Float, Float)
                  , playerDirection :: Float
                  , isMoving :: Bool
                  , isTurning :: (Left, Right)
                  , playerStatus :: PlayerStatus
}

data Asteroid = Asteroid {
                   asteroidPosition :: (Float, Float)
                  , asteroidDirection :: Float
                  , asteroidSpeed :: Float
                  , asteroidVariant :: Variant
}

data Bullet = Bullet {
                 bulletPosition :: (Float, Float)
                , bulletDirection :: Float
                , hit :: Bool
}

initialState :: Int ->  GameState
initialState n = GameState 0 (PlayerInfo (0, 0) 90 False (False, False) Alive) [Asteroid (100,100) 180 1 LargeAsteroid, Asteroid (50, 50) 90 2 SmallAsteroid] [] IsNotPaused n 0