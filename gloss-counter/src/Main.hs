module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

-- | The main entry point
main :: IO ()
main = do
  let window = InWindow "Asteroids" (800, 600) (10, 10)
  let background = black
  let playerCircle = color red (circle 25)
  playIO window background 30 initialState (view playerCircle) input step

-- | Update the game state



              