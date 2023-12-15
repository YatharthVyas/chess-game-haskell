module Piece where

import Graphics.Vty.Attributes
import Lens.Micro ((^.))
import Types
import TestData
import Data.Maybe (isNothing, isJust, fromMaybe)
import qualified Graphics.Vty as V

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
canMove (Piece c Pawn) (x,y) (x', y')     = (abs (y' - y) == 1 && x - x' == direction) || (y == y' && direction * (x - x') <= stepSize && direction * (x - x') > 0)
                                            where stepSize = if (x == 1 && c == white) || (x == 6 && c == black) then 2 else 1
                                                  direction = if c == black then 1 else -1
canMove (Piece _ Knight) (x,y) (x', y')   = (abs (x' - x) == 1 && abs (y' - y) == 2) || (abs (x' - x) == 2 && abs (y' - y) == 1)
-- Bishop can move diagonally eg: [2,3] to [4,5] , [4,1] and so on
canMove (Piece _ Bishop) (x,y) (x', y')   = abs (x' - x) == abs (y' - y)
-- Rook can move horizontally or vertically
canMove (Piece _ Rook) (x,y) (x', y')     = (x' == x && y' /= y) || (x' /= x && y' == y)
-- Queen moves similar to Bishop and Rook
canMove (Piece c Queen) start end = canMove (Piece c Rook) start end || canMove (Piece c Bishop) start end
-- King can move 1 step in any direction
canMove (Piece _ King) (x,y) (x', y')     = abs (x' - x) <= 1 && abs (y' - y) <= 1

getList :: Int -> Int -> [Int]
getList x y = if x > y then reverse [y .. x] else [x .. y]

-- Checks if a linear path (horizontal, vertical or diagonal) is clear
checkLinearPath:: Board -> (Int, Int) -> (Int, Int) -> Dir -> Bool
checkLinearPath b (x,y) (x', y') DirX = let dir = signum (x' - x)
                                            in all (\x1 -> isNothing (getPieceAt b (x1, y))) $ getList (x+dir) (x'-dir)
checkLinearPath b (x,y) (x', y') DirY = let dir = signum (y' - y)
                                            in all (\y1 -> isNothing (getPieceAt b (x, y1))) $ getList (y+dir) (y'-dir)
checkLinearPath b (x,y) (x', y') DirXY = let dirX = signum (x' - x)
                                             dirY = signum (y' - y)
                                             in all (\(x1, y1) -> isNothing (getPieceAt b (x1, y1))) $ zip (getList (x+dirX) (x'-dirX)) (getList (y+dirY) (y'-dirY))

validPawnAttack :: Piece -> (Int, Int) -> (Int, Int) -> Bool
validPawnAttack (Piece c Pawn) (x,y) (x', y') = (x' - x) == direction && abs (y' - y) == 1
                                                      && (c /= endColor) where direction = if c == black then -1 else 1
                                                                               endColor = maybe c getPieceColor (getPieceAt initialBoard (x', y'))

pathClear :: Piece -> Board -> (Int, Int) -> (Int, Int) -> Bool
pathClear p@(Piece c Pawn) b start@(x,y) end@(x', y') = if y == y' then
                                                            if abs (x - x') == 2 then isNothing (getPieceAt b (x + d, y)) && isNothing (getPieceAt b (x + 2*d, y)) else isNothing (getPieceAt b (x + d, y))
                                                            else isJust (getPieceAt b end) && getBoardPieceColor b end /= c
                                                            where d = if c == black then -1 else 1    -- move forward
pathClear (Piece _ Knight) b (x,y) (x', y') = True -- knight can jump
pathClear (Piece _ Bishop) b (x,y) (x', y') = abs (x - x') == 1 || checkLinearPath b (x,y) (x', y') DirXY -- use signum and define XY'
pathClear (Piece _ Rook) b (x,y) (x', y') = abs (x - x') == 1 || abs (y - y') == 1 || checkLinearPath b (x, y) (x', y') d where d = if x == x' then DirY else DirX -- we check abs gap because checkLinearPath needs more than 1 gap
pathClear (Piece c Queen) b (x,y) (x', y') = if x == x' || y == y' then pathClear (Piece c Rook) b (x,y) (x', y')
                                                else  pathClear (Piece c Bishop) b (x,y) (x', y')
pathClear (Piece _ King) b (x,y) (x', y') = True -- king can move 1 step in any direction


-- Use this function to check if any move is legal
isLegalMove:: Board -> (Int, Int) -> (Int, Int) -> Bool
isLegalMove b initial@(x,y) final@(x', y') = case getPieceAt b (x,y) of
                                Nothing -> False
                                Just p -> x' >= 0 && x' < 8 && y' >= 0 && y' < 8    -- check out of bounds
                                            && canMove p initial final
                                            && pathClear p b initial final
                                            && (case getPieceAt b final of     -- final location should be empty or have a piece of opposite color
                                                    Nothing -> True
                                                    Just p' -> getPieceColor p /= getPieceColor p'
                                                    -- castling
                                                    || (getPieceAt b final == Just (Piece (getPieceColor p) King)
                                                    && getPieceAt b initial == Just (Piece (getPieceColor p) Rook)))

-- Get possible Moves for a piece
getPossibleMoves :: Board -> (Int, Int) -> [(Int, Int)]
getPossibleMoves b (x,y) = filter (isLegalMove b (x,y)) [(x', y') | x' <- [0..7], y' <- [0..7]]

getBoardPieceColor :: Board -> (Int, Int) -> Color
getBoardPieceColor b (x,y) = getPieceColor $ fromMaybe (Piece blue Pawn) (getPieceAt b (x,y))


isKingCheck :: Board -> Player -> Bool
isKingCheck board player =
  -- traverse the board and find the king of the opponent player
  let opponent = if player == White then Black else White
      kingPos = [(rank, file) | rank <- [0..7], file <- [0..7], case getPieceAt board (rank, file) of
                                                                  Just pc -> case pc ^. pieceType of
                                                                                King -> compareColorPlayer (pc ^. pieceColor) opponent
                                                                                _ -> False
                                                                  _ -> False]
      result = head kingPos
      pieces = [(rank, file) | rank <- [0..7], file <- [0..7], case getPieceAt board (rank, file) of
                                                                  Just pc -> compareColorPlayer (pc ^. pieceColor) player
                                                                  _ -> False]
      possibleMoves = [True | start <- pieces, isLegalMove board start result]
  in if length possibleMoves > 0 then True else False

-- Attacker, Victim
getLineOfAttack :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getLineOfAttack (x,y) (x', y')
      | x == x' && y /= y' = [(x, y'') | y'' <- [min y (y' - dirY) .. max y (y' - dirY)]]
      | x /= x' && y == y' = [(x'', y) | x'' <- [min x (x' - dirX) .. max x (x' - dirX)]]
      | x /= x' && y /= y' = [(x'', y'') | (x'', y'') <- zip [x, x+dirX .. x'-dirX] [y, y+dirY .. y'-dirY]]
      | otherwise = []
      where dirX = signum (x' - x)
            dirY = signum (y' - y)

compareColorPlayer :: V.Color -> Player -> Bool
compareColorPlayer c p
  | c == V.black && p == Black = True
  | c == V.white && p == White = True
  | otherwise = False

-- >>> getBoardPieceColor initialBoard (0,0) == white
-- True

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

-- >>> getPieceAt initialBoard (1,4)
-- Just (Piece {_pieceColor = ISOColor 7, _pieceType = Pawn})

-- >>> isLegalMove initialBoard (1,4) (2,4)
-- True
--

-- >>> checkLinearPath initialBoard (0,3) (0,5) DirX
-- False

-- >>> isLegalMove initialBoard (0,0) (2,0)
-- True

-- >>> pathClear (Piece black Rook) initialBoard (0,0) (2,0)
-- False

-- >>> checkLinearPath initialBoard (0,0) (2,0) DirX
-- False

-- >>> pathClear (Piece white Bishop) initialBoard (0,5) (4,1)
-- False

