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
movePiece _ _ = selectPosition Nothing
