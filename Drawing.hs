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

module Drawing (drawGame, createViewport, screenToPosition) where

import Control.Monad.State
import qualified Data.Map as Map
import Graphics.Gloss

import Game

positionToScreen :: Viewport -> Position -> Point
positionToScreen (Viewport left top width) (xx, yy)  =
    let spacing = width / 6.0
        xx' = left + spacing * fromIntegral xx
        yy' = top + spacing * fromIntegral yy
    in (xx', yy')

screenToPosition :: (Float, Float) -> GameState -> Position
screenToPosition (sx, sy) game =
  let (Viewport left top width) = viewport game
      spacing = width / 6.0
      xx = round $ (sx - left) / spacing
      yy = round $ (sy - top) / spacing
  in (xx, yy)

drawPiece :: GameState -> Position -> Side -> Picture

drawPiece game pos piece =
    let (cx, cy) = positionToScreen (viewport game) pos
        arm = rectangleSolid 5 60
    in case piece of
         Fox -> Color blue $ Translate cx cy $ ThickCircle 30 5
         Goose -> Color red $ Translate cx cy $ Pictures [Rotate 45 arm, Rotate (-45) arm]

drawGrid :: GameState -> Picture
drawGrid game =
    let (Viewport left top width) = viewport game
        spacing = width / 6.0
        lines = Pictures $ map Line $ concat [[[(xx, (-spacing)), (xx, spacing)]
                                                   | xx <- [(-spacing), 0, spacing]],
                                              [[(-spacing, -spacing), (spacing, spacing)]]]
        subgrid = Pictures $ [lines, Rotate 90 lines]
        subgridSpacing = width / 3.0

        drawSubgrid (ox, oy) = Translate (left + center ox) (top + center oy) subgrid
            where center offs = offs * subgridSpacing + spacing

    in Pictures $ map drawSubgrid [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]

drawSelection :: GameState -> Picture
drawSelection game =
    case selectedPos game of
      Nothing -> blank
      Just pos ->
          let (cx, cy) = positionToScreen (viewport game) pos
          in Translate cx cy $ rectangleSolid 10 10
                          
drawGame :: GameState -> Picture
drawGame game = Pictures [drawGrid game, pieces, drawSelection game]
    where pieces = Pictures $ Map.foldrWithKey (\k v a -> drawPiece game k v : a) []  (board game)

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
