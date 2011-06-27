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

module Game where

import Debug.Trace
import Control.Monad.State
import Data.Functor ((<$>))
import qualified Data.Map as Map

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
                             board :: Board,
                             selectedPos :: Maybe Position,
                             currentPlayer :: Side
                           }

type GameStateM a = State GameState a

validPositions = filter (not . invalid) [(x, y) | x <- [0..6], y <- [0..6]]
    where invalid (x, y) = or [x < 2 && y < 2,
                               x > 4 && y < 2,
                               x < 2 && y > 4,
                               x > 4 && y > 4]

initBoard = Map.fromList . concat $
            [zip (filter (\(x, y) -> y < 4) validPositions) (repeat Goose),
             zip (zip [0, 1, 5, 6] (repeat 4)) (repeat Goose),
             [((2, 5), Fox), ((4, 5), Fox)]]

adjacentPositions :: Position -> [Position]
adjacentPositions (px, py) = filter onBoard . map addOffset $ offsets
    where onBoard = (`elem` validPositions)
          offsets = filter adjacent [(x, y) | x <- [-1..1], y <- [-1..1]]
          adjacent delta@(dx, dy) = not (delta == (0, 0) ||
                                         (odd (px + py) && dx /= 0 && dy /= 0))
          addOffset (dx, dy) = (px + dx, py + dy)

getPiece :: Position -> GameState -> Maybe Side
getPiece pos = getPiece' pos . board

getPiece' :: Position -> Board -> Maybe Side
getPiece' = Map.lookup

selectPosition :: Maybe Position -> GameStateM ()
selectPosition pos = modify (\game -> game {selectedPos = pos})

-- Move a piece.  This will fail if the 'from' position is empty.
movePiece :: Position -> Position -> GameStateM ()
movePiece from to = do
  (valid, jumped) <- isMoveValid from to
  when valid $ do
    oldBoard <- gets board
    let newBoard = jumpPiece jumped . move $ oldBoard
    modify $ \game -> game {board = newBoard}
    switchPlayer
  selectPosition Nothing

    where move board' = maybe board' (boardAfterMove board') $ getPiece' from board'

          -- If a piece was jumped, remove it from the board
          jumpPiece Nothing = id
          jumpPiece (Just pos) = Map.delete pos

          boardAfterMove board' piece = Map.insert to piece . Map.delete from $ board'

          switchPlayer = modify $ \game ->
                 case currentPlayer game of
                   Fox ->   game {currentPlayer = Goose}
                   Goose -> game {currentPlayer = Fox}

-- Test if a move is valid. This will fail if the 'from' position is empty
isMoveValid :: Position -> Position -> GameStateM (Bool, Maybe Position)
isMoveValid from@(fx, fy) to@(tx, ty) = do
  Just movingPiece <- gets $ getPiece from
  destinationIsEmpty <- (not . Map.member to) <$> gets board
  isJump <- isGoose . jumpedPos $ movingPiece

  valid <- return $ and [onBoard,
                         movingPiece == Fox || movingForward,
                         destinationIsEmpty,
                         isAdjacent || isJump]

  return (valid, jumpedPos movingPiece)

      where
        dx = tx - fx
        dy = ty - fy

        onBoard = to `elem` validPositions

        isAdjacent = to `elem` adjacentPositions from

        movingForward = dy >= 0

        -- Given the piece doing the jumping, returns the position jumped if it's valid
        jumpedPos Fox
            | dx == 0 && abs dy == 2                        = Just (fx, fy + signum dy)
            | dy == 0 && abs dx == 2                        = Just (fx + signum dx, fy)
            | even (fx + fy) && abs dx == 2 && abs dy == 2  = Just (fx + signum dx, fy + signum dy)
        jumpedPos _ = Nothing

        isGoose Nothing = return False
        isGoose (Just pos) = (Just Goose ==) <$> (gets $ getPiece pos)
