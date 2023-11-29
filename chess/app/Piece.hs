{-# LANGUAGE TemplateHaskell #-}

module Piece where

import Graphics.Vty.Attributes
import Lens.Micro ((^.))
import Types

---------------------------------------
------  Piece Getter functions  -------
---------------------------------------

getPieceAt :: Board -> (Int, Int) -> Maybe Piece
getPieceAt b (x, y) = (b !! x !! y) ^. cellPiece
-- Note: !! is the list indexing operator (eg: [0,1,2,3] !! 2 = 2)
--       ^. is the lens operator to get a value from a lens (eg: person.name = "John" can be written as person ^. name)

getPieceColor:: Piece -> PlayerColor
getPieceColor p = p ^._pieceColor


---------------------------------------
-----  Piece Movement functions  ------
---------------------------------------

-- @TODO: check if path is blocked
canMove :: Piece -> (Int, Int) -> Bool
-- Pawn can move 1 step forward or 2 steps forward if it is in its initial position
canMove (Piece c Pawn (x, y)) (x', y') = x == x' && (y' == y + 1 || y' == y + 2) -- @TODO: need to check color and subtract if diff color
canMove (Piece _ Knight (x, y)) (x', y') = x' >= 0 && x' < 8 && y' >= 0 && y' < 8 && (
                                            (abs (x' - x) == 1 && abs (y' - y) == 2) ||
                                            (abs (x' - x) == 2 && abs (y' - y) == 1))
-- Bishop can move diagonally eg: [2,3] to [4,5] , [4,1] and so on
canMove (Piece _ Bishop (x, y)) (x', y') = x' >= 0 && x' < 8 && y' >= 0 && y' < 8 && abs (x' - x) == abs (y' - y)
-- Rook can move horizontally or vertically
canMove (Piece _ Rook (x, y)) (x', y') = x' >= 0 && x' < 8 && y' >= 0 && y' < 8 && (
                                           (x' == x && y' /= y) ||
                                           (x' /= x && y' == y))
-- Queen moves similar to Bishop and Rook
canMove (Piece c Queen (x, y)) (x', y') = canMove (Piece c Rook (x, y)) (x', y') || canMove (Piece c Bishop (x, y)) (x', y')
-- King can move 1 step in any direction
canMove (Piece _ King (x, y)) (x', y') = x' >= 0 && x' < 8 && y' >= 0 && y' < 8 && abs (x' - x) <= 1 && abs (y' - y) <= 1