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

module Input (handleInputEvent) where

import Debug.Trace
import Control.Monad.State

import Graphics.Gloss.Interface.Game

import Game
import Drawing (screenToPosition)

click :: Position -> GameStateM ()
click pos = do
  selected <- gets selectedPos
  clickedPiece <- gets $ getPiece pos
  player <- gets currentPlayer
  case selected of
    Nothing -> case clickedPiece of
                 Nothing -> selectPosition Nothing
                 Just piece -> when (piece == player)
                                 (selectPosition $ Just pos)
    Just fromPos -> movePiece fromPos pos

dispatchEvent :: Event -> GameStateM ()
dispatchEvent (EventKey (MouseButton LeftButton) Up _ pnt) =
    gets (screenToPosition pnt) >>= click
dispatchEvent _ = return ()

handleInputEvent :: Event -> GameState -> GameState
handleInputEvent event game = execState (dispatchEvent event) game
