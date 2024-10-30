-- | This module contains the data types
--   which represent the state of the game
module Model where

type Left = Bool
type Right = Bool

data Variant = SmallAsteroid | MediumAsteroid | LargeAsteroid

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
}

data PlayerInfo = PlayerInfo {
                    playerPosition :: (Float, Float)
                  , playerDirection :: Float
                  , isMoving :: Bool
                  , isTurning :: (Left, Right)
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
}

initialState :: GameState
initialState = GameState 0 (PlayerInfo (0, 0) 90 False (False, False)) [Asteroid (100,100) 180 0 LargeAsteroid] [] IsNotPaused

