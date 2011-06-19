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

positionToScreen :: Viewport -> Position -> Point
positionToScreen (Viewport left top width) (xx, yy)  =
    let spacing = width / 6.0
        xx' = left + spacing * fromIntegral xx
        yy' = top + spacing * fromIntegral yy
    in (xx', yy')

drawSide :: GameState -> Position -> Side -> Picture

drawSide game pos Fox =
    let (cx, cy) = positionToScreen (viewport game) pos
    in Color blue $ Translate cx cy $ ThickCircle 30 5
    
drawSide game pos Goose =
    let (cx, cy) = positionToScreen (viewport game) pos
        arm = rectangleSolid 5 60
    in Color red $ Translate cx cy $ Pictures [Rotate 45 arm, Rotate (-45) arm]

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
drawGame game@(GameState vp brd) = Pictures [drawGrid game, pieces]
    where pieces = Pictures $ Map.foldrWithKey (\k v a -> drawSide game k v : a) []  brd

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
    in GameState (Viewport left top dim) (Map.fromList [((2, 2), Fox),
                                                        ((3, 4), Fox),
                                                        ((3, 1), Goose)])

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
