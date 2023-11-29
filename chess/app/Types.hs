{-# LANGUAGE TemplateHaskell #-}

module Types where

import Graphics.Vty.Attributes
import Lens.Micro
import Lens.Micro.TH (makeLenses)

data PlayerColor = Black | White
data Cell = Cell { _cellColor :: Color, _cellPiece :: Maybe Piece }
data Piece = Piece { _pieceColor :: PlayerColor, _pieceType :: PieceType, _piecePosition :: (Int, Int) }
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King

type Board = [[Cell]]

makeLenses ''Cell
makeLenses ''Piece
-- makeLenses ''Board
