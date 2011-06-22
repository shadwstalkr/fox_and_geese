module Game where

import Debug.Trace
import Control.Monad.State
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

selectPosition :: Maybe Position -> GameStateM ()
selectPosition pos = modify (\game -> game {selectedPos = pos})

getPiece :: Position -> GameState -> Maybe Side
getPiece pos = Map.lookup pos . board

movePiece :: Position -> Position -> GameStateM ()
movePiece from to = do
  (valid, jumped) <- isMoveValid from to
  when valid $ do
    move jumped
    switchPlayer
  selectPosition Nothing

    where move jumped =  do
            Just piece <- gets $ getPiece from
            moveBoard <- gets $ Map.insert to piece . Map.delete from . board
            -- If a piece was jumped, remove it from the board
            let newBoard = case jumped of
                             Nothing -> moveBoard
                             Just pos -> Map.delete pos moveBoard
            modify $ \game -> game {board = newBoard}

          switchPlayer = modify $ \game ->
                 case currentPlayer game of
                   Fox ->   game {currentPlayer = Goose}
                   Goose -> game {currentPlayer = Fox}


isMoveValid :: Position -> Position -> GameStateM (Bool, Maybe Position)
isMoveValid from@(fx, fy) to@(tx, ty) = do
  Just movingPiece <- gets $ getPiece from

  destinationIsEmpty <- gets $ not . Map.member to . board
  let dx = tx - fx
      dy = ty - fy

      onBoard = to `elem` validPositions

      gooseMovesForward = movingPiece == Fox || dy >= 0

      adjacent = to `elem` adjacentPositions from

      jumpedPos = getJumpedPos movingPiece from dx dy

      getJumpedPos Fox (fx, fy) dx dy
          | dx == 0 && abs dy == 2 = Just (fx, fy + sign dy)
          | dy == 0 && abs dx == 2 = Just (fx + sign dx, fy)
          | even (fx + fy) && abs dx == 2 && abs dy == 2 = Just (fx + sign dx, fy + sign dy)
      getJumpedPos _ _ _ _ = Nothing

      sign n
          | n < 0 = -1
          | otherwise = 1

  jump <- case jumpedPos of
            Nothing -> return False
            Just pos -> do
              jumpedPiece <- gets $ getPiece pos
              return $ case jumpedPiece of
                         Just Goose -> True
                         otherwise -> False

  valid <- return $ and [onBoard, gooseMovesForward, destinationIsEmpty, adjacent || jump]

  return (valid, jumpedPos)

