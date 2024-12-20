{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE InstanceSigs #-}
-- | This module defines how to turn
--   the game state into a picture
--asteroids
module View where

import Graphics.Gloss
import Model
import Controller
import Deletion
import Collision (playerCollisions)
import Score

class Draw a where
    draw :: Picture -> a -> Picture

--For drawing the player
instance Draw PlayerInfo where
  draw pic (PlayerInfo (x, y) angle _ _ _ _ _) = translate x y $ rotate angle pic

--For drawing asteroids
instance Draw Asteroid where
  draw :: Picture -> Asteroid -> Picture
  draw pic (Asteroid (x, y) _ _ SmallAsteroid _ _)  = translate x y pic
  draw pic (Asteroid (x, y) _ _ MediumAsteroid _ _) = translate x y (scale 2 2 pic)
  draw pic (Asteroid (x, y) _ _ LargeAsteroid _ _)  = translate x y (scale 3 3 pic)
  draw pic (Asteroid (x, y) _ _ AlienAsteroid _ _)  = translate x y pic
  draw _ (Asteroid _  _ _ Destroyed _ _)            = blank

--For drawing a bullet
instance Draw Bullet where
  draw pic (Bullet (x, y) _ _) = translate x y pic

--Used to animate an explosion for when an asteroid is destroyed
animateExplosion :: [Picture] -> Asteroid -> Picture
animateExplosion [p0, p1, p2, p3, p4, p5, p6, p7] (Asteroid (x, y) _ _ Destroyed _ 0) = translate x y p0
animateExplosion [p0, p1, p2, p3, p4, p5, p6, p7] (Asteroid (x, y) _ _ Destroyed _ 1) = translate x y p1
animateExplosion [p0, p1, p2, p3, p4, p5, p6, p7] (Asteroid (x, y) _ _ Destroyed _ 2) = translate x y p2
animateExplosion [p0, p1, p2, p3, p4, p5, p6, p7] (Asteroid (x, y) _ _ Destroyed _ 3) = translate x y p3
animateExplosion [p0, p1, p2, p3, p4, p5, p6, p7] (Asteroid (x, y) _ _ Destroyed _ 4) = translate x y p4
animateExplosion [p0, p1, p2, p3, p4, p5, p6, p7] (Asteroid (x, y) _ _ Destroyed _ 5) = translate x y p5
animateExplosion [p0, p1, p2, p3, p4, p5, p6, p7] (Asteroid (x, y) _ _ Destroyed _ 6) = translate x y p6
animateExplosion [p0, p1, p2, p3, p4, p5, p6, p7] (Asteroid (x, y) _ _ Destroyed _ 7) = translate x y p7
animateExplosion _  _                                                                 = blank

--To display the screen
view :: Picture -> Picture -> Picture -> Picture -> Picture -> [Picture] -> GameState -> Picture
view background playerCircle asteroidBlock alienBlock bulletDot explosions gs@(GameState _ playerInfo asteroids bullets _ _ _ scores) = pictures
  [
      background
    , showInfo gs
    , draw playerCircle playerInfo
    , pictures $ map (\a@(Asteroid _ _ _ variant _ _) -> if variant == AlienAsteroid then draw alienBlock a else draw asteroidBlock a) asteroids
    , pictures $ map (animateExplosion explosions) $ filter (\a@(Asteroid _ _ _ variant _ _) -> variant == Destroyed ) asteroids
    , pictures $ map (draw bulletDot) (removeBullets bullets)
    , showStartScreen gs
    , showPauseScreen gs
    , showGameOverScreen gs
    , showSavingScoreScreen gs
    , showHighScoreScreen gs
  ]

--Shows some general info in the topleft, such as score, time, and amount of lives
showInfo :: GameState -> Picture
showInfo (GameState time player _ _ _ _ _ _) = Pictures
  [
      translate (-390) 280 $ scale 0.1 0.1 $ color white $ text ("Score: " ++ show (playerScore player))
    , translate (-390) 240 $ scale 0.1 0.1 $ color white $ text ("Time: " ++ show (truncate time))
    , translate (-390) 200 $ scale 0.1 0.1 $ color white $ text ("Life: " ++ show (lives player))
  ]

--Start screen, shown at the start of the game
startScreen :: Picture
startScreen = pictures [color white $ translate (-75) 0 $ scale 0.1 0.1 $ text "Press Enter to start"]

--Pause screen, shown when the game is paused
pauseScreen :: Picture
pauseScreen = pictures [color white $ translate (-100) 0 $ scale 0.3 0.3 $ text "Paused", color white $ translate (-100) (-50) $ scale 0.1 0.1 $ text "Press 'p' to unpause"]

-- game over screen if player is dead. player should be able to type in player name and then save with enter.
gameOverScreen :: Picture
gameOverScreen = pictures [color white $ translate (-100) 0 $ scale 0.3 0.3 $ text "Game Over", color white $ translate (-100) (-50) $ scale 0.1 0.1 $ text "Press Enter to continue:"]

--Enter name screen
enterNameScreen :: PlayerInfo -> Picture
enterNameScreen (PlayerInfo _ _ _ _ _ _ name) = Pictures
    [
          color black $ rectangleSolid 800 600
        , translate (-250) 50 $ scale 0.2 0.2 $ color white $ text "Type in your name and press 'Enter':"
        , translate (-25) 0 $ scale 0.2 0.2 $ color white $ text name
    ]

-- Show black screen with high scores 1 to 5  
highScoreScreen :: ScoreList -> Picture
highScoreScreen scores = Pictures
    [
          color black $ rectangleSolid 800 600
        , translate (-100) 150 $ scale 0.2 0.2 $ color white $ text "High Scores:"
        , showHighScores scores
    ]

--Shows the starting screen
showStartScreen :: GameState -> Picture
showStartScreen (GameState _ _ _ _ NotStarted _ _ _) = startScreen
showStartScreen _                                    = blank

--Shows the Game Over screen
showGameOverScreen :: GameState -> Picture
showGameOverScreen (GameState _ p@(PlayerInfo _ _ _ _ Dead _ _)  _ _ GameOver _ _ _) =  gameOverScreen
showGameOverScreen _                                                                 = blank

--Shows the savingscore screen
showSavingScoreScreen :: GameState -> Picture
showSavingScoreScreen (GameState _ p@(PlayerInfo _ _ _ _ Dead _ _)  _ _ SavingScore _ _ _) = enterNameScreen p
showSavingScoreScreen _                                                                    = blank

--Shows the pause screen
showPauseScreen :: GameState -> Picture
showPauseScreen (GameState _ _ _ _ Paused _ _ _) = pauseScreen
showPauseScreen _                                = blank

--Shows the highscorescreen
showHighScoreScreen :: GameState -> Picture
showHighScoreScreen (GameState _ _ _ _ HighScores _ _ scores) = highScoreScreen scores
showHighScoreScreen _                                         = blank

--Makes the scorelist in a picture that can be displayed
showHighScores :: ScoreList -> Picture
showHighScores scores = pictures $ zipWith showHighScore scores [1..]

--Makes a score into a picture 
showHighScore :: [String] -> Int -> Picture
showHighScore [name, score] n = translate (-100) (150 - 50 * fromIntegral n) $ scale 0.2 0.2 $ color white $ text $ name ++ " " ++ show score