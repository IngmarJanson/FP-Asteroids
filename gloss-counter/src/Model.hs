-- | This module contains the data types
--   which represent the state of the game
module Model where
import GHC.Integer (smallInteger)

type Left = Bool
type Right = Bool

data Variant = Destroyed | SmallAsteroid | MediumAsteroid | LargeAsteroid | Alien
    deriving (Eq, Enum, Bounded)

--Describing the highscores
type ScoreList = [[String]] 

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
                  , highScores :: ScoreList
}

data PlayerInfo = PlayerInfo {
                    playerPosition :: (Float, Float)
                  , playerDirection :: Float
                  , isMoving :: Bool
                  , isTurning :: (Left, Right)
                  , playerStatus :: PlayerStatus
                  , playerScore :: Int
                  , playerName :: String
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

initialState :: Int -> ScoreList -> GameState
initialState n scores = GameState 0 (PlayerInfo (0, 0) 90 False (False, False) Alive 0 "") [] [] IsNotPaused n 0 scores