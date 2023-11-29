{-# LANGUAGE TemplateHaskell #-}

module Types where

import Graphics.Vty.Attributes
import Lens.Micro
import Lens.Micro.TH (makeLenses)

-- data PlayerColor = Black | White
data Cell = Cell { _cellColor :: Color, _cellPiece :: Maybe Piece } deriving (Show)
data Piece = Piece { _pieceColor :: Color, _pieceType :: PieceType } deriving (Show)
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show)

type Board = [[Cell]]

makeLenses ''Cell
makeLenses ''Piece
-- makeLenses ''Board
