module Input (handleInputEvent) where

import Graphics.Gloss.Interface.Game

import Game

handleInputEvent :: Event -> GameState -> GameState
handleInputEvent _ = id

