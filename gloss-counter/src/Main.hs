module Main where

import Controller
import Model
import View
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | The main entry point
main :: IO ()
main = do
  let window = InWindow "Asteroids" (800, 600) (10, 10)
  let background = black
  let playerCircle = color red (polygon [(50/3,25),(50/3,-25),(- (100 / 3),0)])
  let asteroidBlock = color white (polygon [(-10, -20),(-15, -10),(-10, -5),(-20, 0),(-10, 10),(-5, 15),(0, 20),(10, 15),(15, 10),(20, 0),(15, -10),(10, -15),(0, -20)])
  let bulletDot = color white (circleSolid 2)
  randomNumber <- randomIO
  playIO window background 30 (initialState randomNumber) (return . view playerCircle asteroidBlock bulletDot) input step
-- | Update the game state