-- | This module contains the data types
--   which represent the state of the game
module Model where

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1

data GameState = GameState {
                   elapsedTime :: Float
                  , playerInfo :: PlayerInfo
}

data PlayerInfo = PlayerInfo {
                    playerPosition :: (Float, Float)
                  , playerDirection :: (Float, Float)
                  , isMoving :: Bool
}

initialState :: GameState
initialState = GameState 0 (PlayerInfo (0, 0) (0, 1) False)