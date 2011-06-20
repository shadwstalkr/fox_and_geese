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
                 Just piece -> if piece == player
                                 then selectPosition $ Just pos
                                 else return ()
    Just fromPos -> movePiece fromPos pos

dispatchEvent :: Event -> GameStateM ()
dispatchEvent (EventKey (MouseButton LeftButton) Up _ pnt) =
    gets (screenToPosition pnt) >>= click
dispatchEvent _ = return ()

handleInputEvent :: Event -> GameState -> GameState
handleInputEvent event game = execState (dispatchEvent event) game
