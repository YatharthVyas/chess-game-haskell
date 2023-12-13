{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}

module Types where

import Graphics.Vty.Attributes
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import Network.Socket
-- data PlayerColor = Black | White
data Cell = Cell { _cellColor :: Color, _cellPiece :: Maybe Piece, _borderColor :: Color } deriving (Show)
data Piece = Piece { _pieceColor :: Color, _pieceType :: PieceType } deriving (Eq, Show)
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show)
data Dir = DirX | DirY | DirXY  -- used to indicate the direction of movement

type Board = [[Cell]]
data Player = White | Black deriving (Show, Eq)
data GameState = GameState {
    board :: Board,
    myPlayer :: Player,
    currentPlayer :: Player,
    userInput :: String,
    lastMove :: String,
    errorMsg :: String,
    connection:: Socket,
    isCheck :: Bool
  } deriving (Show)
makeLenses ''GameState
makeLenses ''Cell
makeLenses ''Piece
-- makeLenses ''Board

-- Note: can avoid lenses