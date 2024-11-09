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
  let window = InWindow "Asteroids" (800, 600) (10, 10)
  background <- loadBMP "assets/background.bmp" 
  let playerCircle = color red (polygon [(50/3,25),(50/3,-25),(- (100 / 3),0)])
  let asteroidBlock = color white (polygon [(-10, -20),(-15, -10),(-10, -5),(-20, 0),(-10, 10),(-5, 15),(0, 20),(10, 15),(15, 10),(20, 0),(15, -10),(10, -15),(0, -20)])
  let alienBlock = color green (polygon [(-10, -20),(-15, -10),(-10, -5),(-20, 0),(-10, 10),(-5, 15),(0, 20),(10, 15),(15, 10),(20, 0),(15, -10),(10, -15),(0, -20)])
  let bulletDot = color white (circleSolid 2)
  -- s <- sequence [loadBMP a]
  randomNumber <- randomIO
  scores <- getHighScores
  playIO window black 30 (initialState randomNumber scores) (return . view background playerCircle asteroidBlock alienBlock bulletDot) input step
-- | Update the game state