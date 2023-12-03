module ValidateMove where

import Data.Char (isDigit, isUpper)
import Types
-- TODO: Even if it was Black turn, I was able to move a white piece. Need to fix this
{-
Algebraic notation for chess moves and their validation
Cases covered:
Captures (e.g., exd5).
Pawn promotions (e.g., e8=Q).
Disambiguating moves where two (or more) identical pieces can move to the same square (e.g., Nbd2, Rae1).
Special moves like castling (O-O for kingside and O-O-O for queenside).
Check (+) and checkmate (#) symbols.
-}
-- isValidChessMove :: String -> Bool
-- isValidChessMove move
--   | move `elem` ["O-O", "O-O-O"] = True  -- Castling
--   | otherwise = case removeCheckCheckmate move of
--       [file, rank] -> isFile file && isRank rank  -- Single-step pawn move
--       [file1, rank1, file2, rank2] -> isFile file1 && isRank rank1 && isFile file2 && isRank rank2 && file1 == file2 && isPawnDoubleStep rank1 rank2  -- Double-step pawn move
--       [piece, file, rank] -> isPiece piece && isFile file && isRank rank
--       [file1, 'x', file2, rank] -> isFile file1 && isFile file2 && isRank rank  -- Pawn capture
--       [piece, file, 'x', file2, rank] -> isPiece piece && isFile file && isFile file2 && isRank rank
--       [piece, file1, rank1, file2, rank2] -> isPiece piece && isFile file1 && isRank rank1 && isFile file2 && isRank rank2  -- Disambiguating move
--       [file, rank, '=', promPiece] -> isFile file && isRank rank && isPromotionPiece promPiece  -- Pawn promotion
--       _ -> False
--   where
--     removeCheckCheckmate s = filter (`notElem` ['+', '#']) s
--     isFile f = f `elem` ['a'..'h']
--     isRank r = r `elem` ['1'..'8']
--     isPiece p = isUpper p && p `elem` "KQRBN"
--     isPromotionPiece p = p `elem` "QRBN"
--     isPawnDoubleStep r1 r2 = (r1 == '2' && r2 == '4') || (r1 == '7' && r2 == '5')


-- Now that a move is valid, we need to write functions which take the Gamestate and the move and update the board if the move is valid
fileToIndex :: Char -> Int
fileToIndex file = fromEnum file - fromEnum 'a'

rankToIndex ::  Char -> Int
rankToIndex rank = fromEnum rank - fromEnum '1'  -- Subtract from 8 for zero-indexing

-- Parses a move string (e.g., "e2e4", "e4", "Nf3") into start and end board indices
parseMove :: Player -> String -> Maybe ((Int, Int), (Int, Int))
parseMove player move
  | length move == 4 =
        let startFile = fileToIndex (move !! 0) -- move[0] = converts e to 4
            startRank = rankToIndex (move !! 1)
            endFile = fileToIndex (move !! 2)
            endRank = rankToIndex (move !! 3)
        in Just ((startRank, startFile), (endRank, endFile))
  | length move == 2 && isPawnMove move = do { -- Handle pawn move (e.g., "e4")
      let startFile = fileToIndex (move !! 0)
          startRank = rankToIndex (move !! 1)
          endFile = fileToIndex (move !! 0)  -- The end file is the same as the start file for pawn moves
          endRank = rankToIndex (move !! 1) + if player == White then 1 else -1 -- Adjust rank for pawn move
      in Just ((startRank, startFile), (endRank, endFile))
  }
  | length move == 3 && isKnightMove move = -- TODO: This is a very imperfect way of parsing knight moves
      let startFile = fileToIndex (move !! 1) -- b or f
          startRank = rankToIndex (move !! 2) --
          {-
          Knight moves in chess are typically represented by their final position, i.e,
          Nf3 means the knight moves to f3. However, if there are two knights that can move to f3,
          then the move is represented as Nbf3, where b is the file of the knight that is moving.

          In our case, currently I am assuming that move is represented as Nb1
          which means take the knight in b file which is at rank 1 and it always gets moved to c3

          What we can do is always use Nbc3 and then we can parse it as follows:
          Take the knight at b file, which is at rank 1 and move it to file c and rank 3
          This will remove ambiguity
          -}
          (deltaRank, deltaFile) = case move !! 0 of
            'N' -> (2, 1)  -- Knight moves have a fixed relative offset
            _ -> (-2, -1)
          endFile = startFile + deltaFile
          endRank = startRank + deltaRank
          in if isValidBoardPosition (startRank + deltaRank) (startFile + deltaFile)
              then Just ((startRank, startFile), (startRank + deltaRank, startFile + deltaFile))
              else Just ((startRank, startFile), (startRank - deltaRank, startFile - deltaFile))
  | otherwise = Nothing

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