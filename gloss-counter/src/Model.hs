-- | This module contains the data types
--   which represent the state of the game
module Model where
import GHC.Integer (smallInteger)

type Left = Bool
type Right = Bool

data Variant = Destroyed | SmallAsteroid | MediumAsteroid | LargeAsteroid | Alien
    deriving (Eq)

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
                  , playerScore :: Int
}

data Asteroid = Asteroid {
                   asteroidPosition :: (Float, Float)
                  , asteroidDirection :: Float
                  , asteroidSpeed :: Float
                  , asteroidVariant :: Variant
                  , asteroidScoreToGive :: Int
}

data Bullet = Bullet {
                 bulletPosition :: (Float, Float)
                , bulletDirection :: Float
                , hit :: Bool
}

initialState :: Int ->  GameState
initialState n = GameState 0 (PlayerInfo (0, 0) 90 False (False, False) Alive 0) [Asteroid (100,100) 0 2 LargeAsteroid 0, Asteroid (400, 400) 0 1 Alien 0] [] IsNotPaused n 0