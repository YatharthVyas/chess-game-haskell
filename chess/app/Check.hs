module Check where

import Graphics.Vty.Attributes
import Lens.Micro ((^.))
import Types
import TestData
import Piece
import ValidateMove (makeMove)
import Data.Maybe (isNothing, isJust, fromMaybe)
import qualified Graphics.Vty as V

-- Attacker, Victim
getLineOfAttack :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getLineOfAttack (x,y) (x', y')
      | x == x' && y /= y' = [(x, y'') | y'' <- [min y (y' - dirY) .. max y (y' - dirY)]]
      | x /= x' && y == y' = [(x'', y) | x'' <- [min x (x' - dirX) .. max x (x' - dirX)]]
      | x /= x' && y /= y' = [(x'', y'') | (x'', y'') <- zip [x, x+dirX .. x'-dirX] [y, y+dirY .. y'-dirY]]
      | otherwise = []
      where dirX = signum (x' - x)
            dirY = signum (y' - y)

possibleMovesForCheck :: Board -> Player -> ((Int, Int), [((Int, Int), (Int, Int))])
possibleMovesForCheck board player =
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
      possibleMoves = [(start, result) | start <- pieces, isLegalMove board start result]

    in (head kingPos, possibleMoves)

isKingCheck :: Board -> Player -> Bool
isKingCheck board player =
  -- traverse the board and find the king of the opponent player
  let(_, possibleMoves) = possibleMovesForCheck board player
  in not (null possibleMoves)

isCheckMate :: Board -> Player -> Bool
isCheckMate board player =
  -- traverse the board and find the king of the opponent player
  let (result, possibleMoves) = possibleMovesForCheck board player
      opponent = if player == White then Black else White

      output = case length possibleMoves of
            0 -> False
            _ -> let (start, _) = head possibleMoves
                     lineOfAttack = getLineOfAttack start result
                     opponentPieces = [(rank, file) | rank <- [0..7], file <- [0..7], case getPieceAt board (rank, file) of
                                                                                 Just pc -> compareColorPlayer (pc ^. pieceColor) opponent
                                                                                 _ -> False]
                     -- check if any of the opponent pieces can defend the king and then run executeMove on that move fillowed by isCheck to ensure the block doesnt create a check
                     isCheckMate = [(start, defendPos) | start <- opponentPieces, defendPos <- lineOfAttack, isLegalMove board start defendPos]
                     move_error = [False | (initialPos, endPos) <- isCheckMate, not (isKingCheck (makeMove board initialPos endPos) player)]

                     -- check if king can move to a safe position
                     kingPossibleMoves = [(result, (endRank, endFile)) | endRank <- [0..7], endFile <- [0..7], isLegalMove board result (endRank, endFile)]
                     kingmove_error = [False | (initialPos, endPos) <- kingPossibleMoves, not (isKingCheck (makeMove board initialPos endPos) player)]

                  in not (not (null move_error) || not (null kingmove_error))
  in output
