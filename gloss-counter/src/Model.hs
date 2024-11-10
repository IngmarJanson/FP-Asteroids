{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where
import GHC.Integer (smallInteger)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1

--Describes the movement of a player
type Left           = Bool
type Right          = Bool

--Describes the variant of an asteroid
data Variant        = Destroyed | SmallAsteroid | MediumAsteroid | LargeAsteroid | AlienAsteroid
    deriving (Eq, Enum, Bounded)

--Describing the highscores
type ScoreList      = [[String]]

--How many lives a player has left
data Lives          = Three | Two | One | Dead
    deriving (Show)

--Describing the hit box as a list of points
type HitBox         = [(Float, Float)]

--Describes if a player is moving
data IsMoving       = Moving | NotMoving | MouseMoving (Float, Float)
    deriving (Show)

data PlayingState   = NotStarted | Playing | Paused | GameOver | SavingScore | HighScores
    deriving (Show, Eq)

--Describes the state of the game
data GameState  = GameState {
                    elapsedTime         :: Float            --Shows the amount of time that the game has been played
                  , playerInfo          :: PlayerInfo       --A lot of info about the player is saved in playerinfo
                  , asteroids           :: [Asteroid]       --A list of all the asteroids
                  , bullets             :: [Bullet]         --A list of all the bullets
                  , playingState        :: PlayingState     --The state of playing, so if the game has paused, if it's playing, etc.
                  , seed                :: Int              --A randomly generated seed used for randomness throughout the game
                  , counter             :: Int              --A counter that goes up each time step taken, used for randomness
                  , highScores          :: ScoreList        --A list with the high scores
}

--A lot of data from the player is saved in here
data PlayerInfo = PlayerInfo {
                    playerPosition      :: (Float, Float)   --The position of a player
                  , playerDirection     :: Float            --The direction of a player in degrees (so from 0 to 360)
                  , isMoving            :: IsMoving         --If a player is moving or not
                  , isTurning           :: (Left, Right)    --If a player is turning and in what direction
                  , lives               :: Lives            --How many lives a player has left 
                  , playerScore         :: Int              --A players score
                  , playerName          :: String           --The name of a player
}

--Describes an asteroid
data Asteroid   = Asteroid {
                    asteroidPosition    :: (Float, Float)   --The position of the asteroid
                  , asteroidDirection   :: Float            --The direction of the asteroid in degrees (so from 0 to 360)
                  , asteroidSpeed       :: Float            --The speed of the asteroid
                  , asteroidVariant     :: Variant          --The variant of the asteroid
                  , asteroidScoreToGive :: Int              --The score that an asteroid will give in a cycle
                  , frameCounter        :: Int              --Used for animating an asteroids explosion
}

data Bullet     = Bullet {
                    bulletPosition      :: (Float, Float)   --The position of the bullet
                  , bulletDirection     :: Float            --The direction of the bullet   
                  , hit                 :: Bool             --If the bullet has hit something
}

-- | The initial state of the game
initialState :: Int -> ScoreList -> GameState
initialState n = GameState 0 (PlayerInfo (0, 0) 90 NotMoving (False, False) Three 0 "") [] [] NotStarted n 0