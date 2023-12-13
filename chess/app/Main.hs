{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use null" #-}

module Main where
import Lens.Micro ((^.))
import Types
import ValidateMove (parseMove, fileToIndex, rankToIndex)
import Piece (isLegalMove, getPieceAt, canMove, pathClear, getBoardPieceColor)
import TestData
import Data.Char (isDigit, isUpper)
{-# LANGUAGE OverloadedStrings #-}
import Brick
import Brick.Main
import Brick.Widgets.Core
import Brick.Widgets.Border.Style
import Brick.Widgets.Border (border,hBorder ,vBorder, borderWithLabel)
import Brick.Widgets.Center (hCenter, vCenter)
import Control.Monad.State
import Data.Monoid ((<>))
import qualified Graphics.Vty as V
 -- Import liftIO
import Control.Monad.IO.Class (liftIO)
import UI

initialGameState :: GameState
initialGameState = GameState initialBoard White "" "" "" False

safeBack :: [a] -> [a]
safeBack [] = []
safeBack xs = init xs


makeMove :: Board -> (Int, Int) -> (Int, Int) -> Board
makeMove board (startRank, startFile) (endRank, endFile) =
  -- Write the code such that we extract the color of the cell of the end position and ensure that it doesnt change when we copy over the start cell
  -- This will ensure that the color of the cell is not changed when we move a piece
    let startCell = (board !! startRank) !! startFile
        endCell = (board !! endRank) !! endFile
        startCellColor = _cellColor startCell
        endCellColor = _cellColor endCell
        -- We want the updated position to have the piece from the start position and the color from the end position
        newStartCell = Cell endCellColor (_cellPiece startCell) endCellColor
        emptyCell = Cell startCellColor Nothing startCellColor   -- Create an empty cell with the same color
        updatedRowStart = take startFile (board !! startRank) ++ [emptyCell] ++ drop (startFile + 1) (board !! startRank)
        updatedBoardStart = take startRank board ++ [updatedRowStart] ++ drop (startRank + 1) board
        updatedRowEnd = take endFile (updatedBoardStart !! endRank) ++ [newStartCell] ++ drop (endFile + 1) (updatedBoardStart !! endRank)
        updatedBoardEnd = take endRank updatedBoardStart ++ [updatedRowEnd] ++ drop (endRank + 1) updatedBoardStart
    in resetHighlightedBoard updatedBoardEnd

executeMove :: GameState -> String -> IO GameState
executeMove gs moveInput =
  case parseMove (currentPlayer gs) moveInput of
    Just (startPos, endPos) ->
      if isLegalMove (board gs) startPos endPos then do
        let newBoard = makeMove (board gs) startPos endPos
            move = userInput gs
            lastm = if length move == 4 then
                (move !! 0) : (move !! 1) : " -> " ++ (move !! 2) : [move !! 3]
            else ""
        return gs { board = newBoard, currentPlayer = if currentPlayer gs == White then Black else White, userInput = "", lastMove = lastm, errorMsg = "Move executed."}
      else do
        return gs { userInput = "", errorMsg="Invalid move. Try again." }
    Nothing -> do
      return gs { userInput = "", errorMsg="Invalid move format." }

compareColorPlayer :: V.Color -> Player -> Bool
compareColorPlayer c p
  | c == V.black && p == Black = True
  | c == V.white && p == White = True
  | otherwise = False

appEvent :: BrickEvent () e -> EventM () GameState ()
appEvent (VtyEvent e) = do
    gs <- get
    case e of
         V.EvKey V.KEsc [] -> halt
         V.EvKey (V.KChar c) [] -> do
             let newInput = userInput gs ++ [c]
             let parsedInput = if length newInput > 1 &&
                      compareColorPlayer (getBoardPieceColor (board gs) (rankToIndex $ newInput !! 1, fileToIndex $ newInput !! 0)) (currentPlayer gs)
                  then Just (fileToIndex $ newInput !! 0, rankToIndex $ newInput !! 1)
                  else Nothing
             case parsedInput of
                  Just (startFile, startRank) -> do
                      let possibleMoves = [(endRank, endFile) | endRank <- [0..7], endFile <- [0..7], isLegalMove (board gs) (startRank, startFile) (endRank, endFile)]
                      let highlightedBoard = highlightPossibleMoves (board gs) possibleMoves
                      put (gs { userInput = newInput, board = highlightedBoard })
                      return ()
                  Nothing -> do
                      put (gs { userInput = newInput })
                      return ()
         -- backspace
         V.EvKey V.KBS [] -> do
             let newInput = safeBack (userInput gs)
             if length newInput < 2 then do
                  put (gs { userInput = newInput, board = resetHighlightedBoard (board gs) })
              else do
                put (gs { userInput = newInput })
             return ()
         V.EvKey V.KEnter [] -> do
             -- Check if the move syntax is valid
             let moveInput = userInput gs

            --  let validSyntax = isValidChessMove moveInput

            --  -- the below lines are messing up the alignment of chessboard
            --  liftIO $ putStrLn $ if validSyntax then "Valid move syntax" else "Invalid move syntax"
             case parseMove (currentPlayer gs) moveInput of
                 Nothing -> put (gs { errorMsg = "Failed to parse move" })
                 Just (startPos, endPos) -> do
            --          liftIO $ putStrLn $ "Start position: " ++ show startPos
            --          liftIO $ putStrLn $ "End position: " ++ show endPos
            --      Nothing -> liftIO $ putStrLn "Failed to parse move"
            --  Execute the move if the syntax is valid
                    let (Just pc) = getPieceAt (board gs) startPos
                    newGameState <- if isLegalMove (board gs) startPos endPos
                                        then if compareColorPlayer (pc ^. pieceColor) (currentPlayer gs)
                                          then liftIO $ executeMove gs moveInput
                                        else do
                                            return gs { errorMsg = "Not your turn!" }
                                    else do
                                      return gs { errorMsg = "Invalid Move" }
                    
                    -- Look if the king is in check to invalidate all other moves 
                    if isCheck newGameState then do
                        let isBool = if currentPlayer gs == White
                                          then isKingCheck (board newGameState) Black
                                      else isKingCheck (board newGameState) White
                        if isBool then
                          put gs { errorMsg = "In Check State!" }
                        else do
                          put newGameState { isCheck = False }

                    -- if king not in check, look if the move puts the opponent king in check
                    else do
                      -- self check
                      let isBool = if currentPlayer gs == White
                                      then isKingCheck (board newGameState) Black
                                    else isKingCheck (board newGameState) White
                      if isBool then
                        put gs { errorMsg = "Invalid Move! Will result in Check" }
                      else do
                        let isBool = isKingCheck (board newGameState) (currentPlayer gs)
                        
                        put newGameState { isCheck = isBool }

                    return ()
         _ -> return ()
appEvent _ = return ()

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

initialBoard2 :: Board
initialBoard2 = [generateRow 0 [Just whiteRook, Just whiteKnight, Just whiteBishop, Just whiteQueen, Just whiteKing, Just whiteBishop, Just whiteKnight, Just whiteRook],
                generateRow 1 [Just whitePawn, Just whitePawn, Just whitePawn, Nothing, Just whitePawn, Just whitePawn, Just whitePawn, Just whitePawn],
                generateRow 2 (replicate 8 Nothing),
                generateRow 3 [Nothing, Just blackBishop, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
                generateRow 4 [Nothing, Nothing, Nothing, Just whitePawn, Nothing, Nothing, Nothing, Nothing], 
                generateRow 5 (replicate 8 Nothing),
                generateRow 6 [Just blackPawn, Just blackPawn, Just blackPawn, Just blackPawn, Nothing, Just blackPawn, Just blackPawn, Just blackPawn],
                generateRow 7 [Just blackRook, Just blackKnight, Just blackBishop, Just blackQueen, Just blackKing, Nothing, Just blackKnight, Just blackRook]
                ]

-- >>> isKingCheck initialBoard2 Black
-- True

app :: App GameState e ()
app =
  App {appDraw = drawUI
      , appChooseCursor = showFirstCursor
      , appHandleEvent = appEvent
      , appStartEvent = return ()
      , appAttrMap = attributeMap
      }


main :: IO ()
main = do
  _ <- defaultMain app initialGameState
  return ()