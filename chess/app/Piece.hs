module Piece where

import Graphics.Vty.Attributes
import Lens.Micro ((^.))
import Types
import TestData
import Data.Maybe (isNothing)

---------------------------------------
------  Piece Getter functions  -------
---------------------------------------

getPieceAt :: Board -> (Int, Int) -> Maybe Piece
getPieceAt b (x, y) = (b !! x !! y) ^. cellPiece
-- Note: !! is the list indexing operator (eg: [0,1,2,3] !! 2 = 2)
--       ^. is the lens operator to get a value from a lens (eg: person.name = "John" can be written as person ^. name)


getPieceColor:: Piece -> Color
getPieceColor p = p ^. pieceColor

---------------------------------------
-----  Piece Movement functions  ------
---------------------------------------

-- Verifies if the move is a legal chess move
canMove :: Piece -> (Int, Int) -> (Int, Int) -> Bool
-- Pawn can move 1 step forward or 2 steps forward if it is in its initial position
canMove (Piece c Pawn) (x,y) (x', y') = (y == y' && direction * (x - x') <= stepSize) || (abs (y' - y) == 1 && direction * (x - x') == 1)
                                            where stepSize = if (x == 1 && c == white) || (x == 6 && c == black) then 2 else 1
                                                  direction = if c == black then 1 else -1
canMove (Piece _ Knight) (x,y) (x', y') = (abs (x' - x) == 1 && abs (y' - y) == 2) || (abs (x' - x) == 2 && abs (y' - y) == 1)
-- Bishop can move diagonally eg: [2,3] to [4,5] , [4,1] and so on
canMove (Piece _ Bishop) (x,y) (x', y') = abs (x' - x) == abs (y' - y)
-- Rook can move horizontally or vertically
canMove (Piece _ Rook) (x,y) (x', y') = (x' == x && y' /= y) || (x' /= x && y' == y)
-- Queen moves similar to Bishop and Rook
canMove (Piece c Queen) (x,y) (x', y') = canMove (Piece c Rook) (x,y) (x', y') || canMove (Piece c Bishop) (x,y) (x', y')
-- King can move 1 step in any direction
canMove (Piece _ King) (x,y) (x', y') = abs (x' - x) <= 1 && abs (y' - y) <= 1

-- Checks if a linear path (horizontal, vertical or diagonal) is clear
checkLinearPath:: Board -> (Int, Int) -> (Int, Int) -> Dir -> Bool
checkLinearPath b (x,y) (x', y') DirX = let dir = signum (x' - x)
                                            in all (\x1 -> isNothing (getPieceAt b (x1, y))) [x + dir .. x' - dir]
checkLinearPath b (x,y) (x', y') DirY = let dir = signum (y' - y)
                                            in all (\y1 -> isNothing (getPieceAt b (x, y1))) [y + dir .. y' - dir]
checkLinearPath b (x,y) (x', y') DirXY = let dirX = signum (x' - x)
                                             dirY = signum (y' - y)
                                             in all (\(x1, y1) -> isNothing (getPieceAt b (x1, y1))) $ zip [x + dirX .. x' - dirX] [y + dirY .. y' - dirY]

pathClear :: Piece -> Board -> (Int, Int) -> (Int, Int) -> Bool
pathClear (Piece c Pawn) b start@(x,y) end@(x', y') = x /= x'                   -- attack move (diagonal)
                                                      || checkLinearPath b start end DirX -- move forward
pathClear (Piece _ Knight) b (x,y) (x', y') = True -- knight can jump
pathClear (Piece _ Bishop) b (x,y) (x', y') = checkLinearPath b (x,y) (x', y') DirXY -- use signum and define XY'
pathClear (Piece _ Rook) b (x,y) (x', y') = checkLinearPath b (min x x', min y y') (max x x', max y y') d where d = if x == x' then DirY else DirX
pathClear (Piece c Queen) b (x,y) (x', y') = if x == x' || y == y' then pathClear (Piece c Rook) b (x,y) (x', y')
                                                else  pathClear (Piece c Bishop) b (x,y) (x', y')
pathClear (Piece _ King) b (x,y) (x', y') = True -- king can move 1 step in any direction


-- Use this function to check if any move is legal
isLegalMove:: Board -> (Int, Int) -> (Int, Int) -> Bool
isLegalMove b initial@(x,y) final@(x', y') = case getPieceAt b (x,y) of
                                Nothing -> False
                                Just p -> x' >= 0 && x' < 8 && y' >= 0 && y' < 8    -- check out of bounds
                                            && canMove p initial final
                                            -- && pathClear p b initial final
                                            && (case getPieceAt b final of     -- final location should be empty or have a piece of opposite color
                                                    Nothing -> True
                                                    Just p' -> getPieceColor p /= getPieceColor p')


-------------------------------------------------
-----   Tests for Piece Movement functions  -----
-------------------------------------------------

-- moving pawn diagonally
-- >>> isLegalMove initialBoard (1,1) (2,2)
-- False

-- >>> getPieceAt initialBoard (1,1)
-- Just (Piece {_pieceColor = ISOColor 7, _pieceType = Pawn})

-- moving pawn 2 steps forward
-- >>> isLegalMove initialBoard (1,1) (3,1)
-- True

-- >>> isLegalMove initialBoard (6,1) (4,1)
-- True

-- >>> isLegalMove initialBoard (6,1) (5,1)
-- True

-- moving pawn 3 steps forward
-- >>> isLegalMove initialBoard (1,1) (4,1)
-- False

-- >>> getPieceAt initialBoard (2,1)
-- Nothing

-- >>> getPieceAt initialBoard (0,1)
-- Just (Piece {_pieceColor = ISOColor 7, _pieceType = Knight})

-- >>> isLegalMove initialBoard (0,1) (2,2)
-- True

-- >>> isLegalMove initialBoard (0,1) (2,3)
-- False

-- >>> isLegalMove initialBoard (0,1) (1,3)
-- False

-- >>> getPieceAt initialBoardWithoutPawn (0,3)
-- Just (Piece {_pieceColor = ISOColor 7, _pieceType = Queen})

-- >>> isLegalMove initialBoardWithoutPawn (0,3) (3,6)
-- True

-- >>> isLegalMove initialBoardWithoutPawn (0,3) (3,0)
-- True

-- >>> isLegalMove initialBoardWithoutPawn (0,3) (3,3)
-- True

-- >>> isLegalMove initialBoardWithoutPawn (0,3) (3,4)
-- False
