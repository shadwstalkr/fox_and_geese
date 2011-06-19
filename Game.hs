module Game where

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
                             selectedPos :: Maybe Position
                           }

type GameStateM a = State GameState a

getSelection :: GameStateM (Maybe Position)
getSelection = get >>= return . selectedPos

selectPosition :: Maybe Position -> GameStateM ()
selectPosition pos = do
  game <- get
  put $ game {selectedPos = pos}
  return ()

getPiece :: Position -> GameStateM (Maybe Side)
getPiece pos = get >>= return . Map.lookup pos . board

movePiece :: Position -> Position -> GameStateM ()
movePiece from to = do
  (valid, jumped) <- isMoveValid from to
  if valid then move else return ()
  selectPosition Nothing

    where move =  do
            game <- get
            Just piece <- getPiece from
            let moveBoard = Map.insert to piece . Map.delete from $ board game
                -- If a piece was jumped, remove it from the board
                newBoard = case jumped of
                             Nothing -> moveBoard
                             Just pos -> Map.delete pos moveBoard
            put $ game {board = newBoard}
            return ()


isMoveValid :: Position -> Position -> GameStateM (Bool, Maybe Position)
--isMoveValid (fx, fy) (tx, ty) = return . all $ [cellEmpty,
--                                                adjacent || jump
isMoveValid _ _ = return (True, Nothing)
