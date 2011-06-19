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
    (Map.fromList [((2, 2), Fox),
                   ((3, 4), Fox),
                   ((3, 1), Goose)])
    Nothing

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
