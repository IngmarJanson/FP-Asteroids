-- | This module contains the data types
--   which represent the state of the game
module Model where

type Left = Bool
type Right = Bool

data Variant = SmallAsteroid | MediumAsteroid | LargeAsteroid

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1

data GameState = GameState {
                   elapsedTime :: Float
                  , playerInfo :: PlayerInfo
                  , asteroids :: [Asteroid]
}

data PlayerInfo = PlayerInfo {
                    playerPosition :: (Float, Float)
                  , playerDirection :: (Float, Float)
                  , isMoving :: Bool
                  , isTurning :: (Left, Right)
}

data Asteroid = Asteroid {
                   asteroidPosition :: (Float, Float)
                  , asteroidDirection :: (Float, Float)
                  , asteroidSpeed :: Float
                  , asteroidVariant :: Variant
}

initialState :: GameState
initialState = GameState 0 (PlayerInfo (0, 0) (0, 1) False (False, False)) []