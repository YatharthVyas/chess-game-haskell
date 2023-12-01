{-# LANGUAGE TemplateHaskell #-}

module Types where

import Graphics.Vty.Attributes
import Lens.Micro
import Lens.Micro.TH (makeLenses)

-- data PlayerColor = Black | White
data Cell = Cell { _cellColor :: Color, _cellPiece :: Maybe Piece } deriving (Show)
data Piece = Piece { _pieceColor :: Color, _pieceType :: PieceType } deriving (Eq, Show)
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show)
data Dir = DirX | DirY | DirXY  -- used to indicate the direction of movement

type Board = [[Cell]]
data Player = White | Black deriving (Show, Eq)
data Game = Game { board :: Board, currentPlayer :: Player, userInput :: String } deriving (Show)
data GameState m = MonadState Game m
makeLenses ''Cell
makeLenses ''Piece
-- makeLenses ''Board

-- Note: can avoid lenses