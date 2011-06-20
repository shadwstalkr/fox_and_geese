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

getSelection :: GameStateM (Maybe Position)
getSelection = get >>= return . selectedPos

selectPosition :: Maybe Position -> GameStateM ()
selectPosition pos = do
  game <- get
  put $ game {selectedPos = pos}
  return ()

getPiece :: Position -> GameStateM (Maybe Side)
getPiece pos = get >>= return . Map.lookup pos . board

getPlayer :: GameStateM Side
getPlayer = get >>= return . currentPlayer

movePiece :: Position -> Position -> GameStateM ()
movePiece from to = do
  (valid, jumped) <- isMoveValid from to
  if valid
    then do
      move jumped
      switchPlayer
    else return ()
  selectPosition Nothing

    where move jumped =  do
            game <- get
            Just piece <- getPiece from
            let moveBoard = Map.insert to piece . Map.delete from $ board game
                -- If a piece was jumped, remove it from the board
                newBoard = case jumped of
                             Nothing -> moveBoard
                             Just pos -> Map.delete pos moveBoard
            put $ game {board = newBoard}
            return ()

          switchPlayer =
              do game <- get
                 case currentPlayer game of
                   Fox -> put $ game {currentPlayer = Goose}
                   Goose -> put $ game {currentPlayer = Fox}


isMoveValid :: Position -> Position -> GameStateM (Bool, Maybe Position)
isMoveValid from@(fx, fy) to@(tx, ty) = do
  game <- get
  Just movingPiece <- getPiece from

  let destinationIsEmpty = not . Map.member to . board $ game
      dx = tx - fx
      dy = ty - fy

      onBoard = to `elem` validPositions

      adjacent = all (`elem` [-1, 0, 1]) [dx, dy] &&
                 (even (tx + ty) || dx == 0 || dy == 0)

      jumpedPos = getJumpedPos movingPiece from dx dy

  jump <- case jumpedPos of
            Nothing -> return False
            Just pos -> do
              jumpedPiece <- getPiece pos
              return $ case jumpedPiece of
                         Just Goose -> True
                         otherwise -> False

  let valid = and [onBoard, destinationIsEmpty, adjacent || jump]
  return (valid, jumpedPos)

      where getJumpedPos Fox (fx, fy) dx dy
                | dx == 0 && abs dy == 2 = Just (fx, fy + sign dy)
                | dy == 0 && abs dx == 2 = Just (fx + sign dx, fy)
                | even (fx + fy) && abs dx == 2 && abs dy == 2 = Just (fx + sign dx, fy + sign dy)
            getJumpedPos _ _ _ _ = Nothing

            sign n
                | n < 0 = -1
                | otherwise = 1
