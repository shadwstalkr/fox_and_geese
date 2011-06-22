{-
Copyright 2011 Alexander Midgley

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Main where

import Debug.Trace
import qualified Data.Map as Map
import Graphics.Gloss.Interface.Game

import Drawing
import Game
import Input

gameStep :: Float -> GameState -> GameState
gameStep _ = id

newGameState :: (Float, Float) -> GameState
newGameState (w, h) =
    GameState
    (createViewport w h)
    initBoard
    Nothing
    Fox

windowSize = (800, 600)
windowPos = (10, 10)

main = gameInWindow
       "Fox & Geese"
       windowSize
       windowPos
       white
       10
       (newGameState ((fromIntegral (fst windowSize)), (fromIntegral (snd windowSize))))
       drawGame
       handleInputEvent
       gameStep
