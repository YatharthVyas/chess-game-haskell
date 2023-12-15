module ValidateMove where

import Data.Char (isDigit, isUpper)
import Types
import qualified Graphics.Vty as V
import Piece
import UI (resetHighlightedBoard)

fileToIndex :: Char -> Int
fileToIndex file = if file >= 'a' && file <= 'h' then fromEnum file - fromEnum 'a' else -1 -- @TODO: Handle this error

rankToIndex ::  Char -> Int
rankToIndex rank = if rank >= '1' && rank <= '8' then fromEnum rank - fromEnum '1' else -1 -- Subtract from 8 for zero-indexing

-- Parses a move string (e.g., "e2e4") - ((1, 4), (3, 4)) into start and end board indices
parseMove :: Player -> String -> Maybe ((Int, Int), (Int, Int))
parseMove player move
  | length move == 4 =
        let startFile = fileToIndex (head move) -- move[0] = converts e to 4
            startRank = rankToIndex (move !! 1) -- move[1] = converts 2 to 1
            endFile = fileToIndex (move !! 2) -- move[2] = converts e to 4
            endRank = rankToIndex (move !! 3) -- move[3] = converts 4 to 3
        in if  (startRank <= 7 && startRank >= 0) &&
               (endRank <= 7 && endRank >= 0) &&
               (startFile <= 7 && startFile >= 0) &&
               (endFile <= 7 && endFile >= 0)
              then Just ((startRank, startFile), (endRank, endFile))
            else Nothing

  | otherwise = Nothing

makeMove :: Board -> (Int, Int) -> (Int, Int) -> Board
makeMove board (startRank, startFile) (endRank, endFile) =
  -- Write the code such that we extract the color of the cell of the end position and ensure that it doesnt change when we copy over the start cell
  -- This will ensure that the color of the cell is not changed when we move a piece
    let startCell = (board !! startRank) !! startFile
        endCell = (board !! endRank) !! endFile
        startCellColor = _cellColor startCell
        endCellColor = _cellColor endCell
        startPieceColor = case _cellPiece startCell of
            Just (Piece c _) -> c
            Nothing -> V.blue
        -- castling
        newStartCellPiece = if getPieceAt board (endRank, endFile) == Just (Piece startPieceColor King) then Just (Piece startCellColor King) else Nothing
        -- We want the updated position to have the piece from the start position and the color from the end position
        newStartCell = Cell endCellColor (_cellPiece startCell) endCellColor
        emptyCell = Cell startCellColor newStartCellPiece startCellColor   -- Create an empty cell with the same color
        updatedRowStart = take startFile (board !! startRank) ++ [emptyCell] ++ drop (startFile + 1) (board !! startRank)
        updatedBoardStart = take startRank board ++ [updatedRowStart] ++ drop (startRank + 1) board
        updatedRowEnd = take endFile (updatedBoardStart !! endRank) ++ [newStartCell] ++ drop (endFile + 1) (updatedBoardStart !! endRank)
        updatedBoardEnd = take endRank updatedBoardStart ++ [updatedRowEnd] ++ drop (endRank + 1) updatedBoardStart
    in resetHighlightedBoard updatedBoardEnd

-- Check if the move is a valid pawn move (e.g., "e4")
isPawnMove :: String -> Bool
isPawnMove [file, rank] = isFile file && isRank rank
isPawnMove _ = False

-- Check if the move is a valid knight move (e.g., "Nf3")
isKnightMove :: String -> Bool
isKnightMove (piece : rest) = piece == 'N' && isFile (head rest) && isRank (last rest)
isKnightMove _ = False

-- Check if a character represents a valid file (a-h)
isFile :: Char -> Bool
isFile c = c `elem` ['a'..'h']

-- Check if a character represents a valid rank (1-8)
isRank :: Char -> Bool
isRank c = c `elem` ['1'..'8']

-- Check if a board position is valid (within the board boundaries)
isValidBoardPosition :: Int -> Int -> Bool
isValidBoardPosition rank file = rank >= 0 && rank <= 7 && file >= 0 && file <= 7