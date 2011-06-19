module Drawing (drawGame, createViewport) where

import qualified Data.Map as Map
import Graphics.Gloss

import Game

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
        lines = Pictures $ map Line $ concat [[[(xx, (-spacing)), (xx, spacing)]
                                                   | xx <- [(-spacing), 0, spacing]],
                                              [[(-spacing, -spacing), (spacing, spacing)]]]
        subgrid = Pictures $ [lines, Rotate 90 lines]
        subgridSpacing = width / 3.0

        drawSubgrid (ox, oy) = Translate (left + center ox) (top + center oy) subgrid
            where center offs = offs * subgridSpacing + spacing

    in Pictures $ map drawSubgrid [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
                          
drawGame :: GameState -> Picture
drawGame game@(GameState vp brd) = Pictures [drawGrid game, pieces]
    where pieces = Pictures $ Map.foldrWithKey (\k v a -> drawSide game k v : a) []  brd

createViewport :: Float -> Float -> Viewport
createViewport w h =
    let xcenter = 0
        ycenter = 0
        margin = 10
        dim = if w < h
              then w - (2 * margin)
              else h - (2 * margin)
        left = xcenter - (dim / 2.0)
        top = ycenter - (dim / 2.0)
    in Viewport left top dim
