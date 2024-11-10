module Main where

import Graphics.Gloss

import Controller
import Model
import View
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Score
-- | The main entry point
main :: IO ()
main = do
  let window          = InWindow "Asteroids" (800, 600) (10, 10)
  background          <- loadBMP "assets/background.bmp" 
  alienBlock          <- loadBMP "assets/alien.bmp"
  playerCircle        <- loadBMP "assets/player.bmp"
  s                   <- sequence [loadBMP "assets/explosion_1.bmp", loadBMP "assets/explosion_2.bmp", loadBMP "assets/explosion_3.bmp", loadBMP "assets/explosion_4.bmp", loadBMP "assets/explosion_5.bmp", loadBMP "assets/explosion_6.bmp", loadBMP "assets/explosion_7.bmp",  loadBMP "assets/explosion_8.bmp"]
  let asteroidBlock   = color (mixColors 0.5 0.5 black white) (polygon [(-10, -20),(-15, -10),(-10, -5),(-20, 0),(-10, 10),(-5, 15),(0, 20),(10, 15),(15, 10),(20, 0),(15, -10),(10, -15),(0, -20)])
  let bulletDot       = color white (circleSolid 2)
  randomNumber        <- randomIO
  scores              <- getHighScores
  playIO window black fps (initialState randomNumber scores) (return . view background playerCircle asteroidBlock alienBlock bulletDot s) input step

fps :: Int
fps = 120