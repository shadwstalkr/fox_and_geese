module Main where

import Debug.Trace
import qualified Data.Map as Map
import Graphics.Gloss.Interface.Game

data Side = Fox | Goose
            deriving (Eq, Show)

type Position = (Int, Int)

type Board = Map.Map Position Side

-- Top-left corner and width.  The viewport is always square.
data Viewport = Viewport { viewportLeft :: Float,
                           viewportTop :: Float,
                           viewportWidth :: Float
                         }

data GameState = GameState { viewport :: Viewport,
                             board :: Board
                           }

horizontalLine left right yy = line [(left, yy), (right, yy)]

verticalLine top bottom xx = line [(xx, top), (xx, bottom)]

{-
drawBorder :: Viewport -> Picture
drawBorder (Viewport left top width) =
    let right = left + width
        bottom = top + width
    in line [(left, top), (right, top), (right, bottom), (left, bottom), (left, top)]
-}

drawGrid :: GameState -> Picture
drawGrid (GameState (Viewport left top width) _) =
    let spacing = width / 6.0
        offsets = map ((+ left) . (* spacing)) [0..6]
        lineHeights = map getLineHeights [0..6]

        getLineHeights idx
            | idx < 2 || idx > 4 = [top + spacing * 2, top + spacing * 4]
            | otherwise = [top, top + width]

        linePts = [zip (repeat offset) lineHeight | (offset, lineHeight) <- zip offsets lineHeights]
        lines = map line linePts

    in Pictures [Pictures lines, Pictures (map (Rotate 90) lines)]

drawGame :: GameState -> Picture
drawGame game@(GameState vp brd) = Pictures [drawGrid game]

handleInputEvent :: Event -> GameState -> GameState
handleInputEvent _ = id

gameStep :: Float -> GameState -> GameState
gameStep _ = id

newGameState :: (Float, Float) -> GameState
newGameState (w, h) =
    let xcenter = 0
        ycenter = 0
        margin = 10
        dim = if w < h
              then w - (2 * margin)
              else h - (2 * margin)
        left = xcenter - (dim / 2.0)
        top = ycenter - (dim / 2.0)
    in GameState (Viewport left top dim) Map.empty

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
