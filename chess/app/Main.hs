{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use null" #-}

module Main where
import Lens.Micro ((^.))
import Types
import Server (createServer, isPortAvailable)
import Client (connectToServer)
import qualified Data.ByteString.Char8 as C
import Control.Concurrent (forkIO)
import ValidateMove (parseMove, fileToIndex, rankToIndex)
import Piece (isLegalMove, getPieceAt, canMove, pathClear, getBoardPieceColor, isKingCheck, compareColorPlayer)
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
import Network.Socket.ByteString (recv, sendAll)
 -- Import liftIO
import Control.Monad.IO.Class (liftIO)
import UI

{-
1. We  probably run it such that it checks if the socket 8080 is being used or not, if it isnt then the terminal which checked it becomes the host and will be white. It will create a socket which will accept connections at port 8080

2. On the other hand, when the second terminal sees that socket 8080 is already bound, then it will send a connection request to that socket and connect to it and will take the color as black

3. As soon as the first terminal receives the request, the game will start and chessboard shows up on both terminals

-}

initialGameState :: GameState
initialGameState = GameState initialBoard White White "" "" "" undefined False

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
        startPieceColor = case _cellPiece startCell of
            Just (Piece c _) -> c
            Nothing -> V.blue
        newStartCellPiece = if getPieceAt board (endRank, endFile) == Just (Piece startPieceColor King) then Just (Piece startCellColor King) else Nothing
        -- We want the updated position to have the piece from the start position and the color from the end position
        newStartCell = Cell endCellColor (_cellPiece startCell) endCellColor
        emptyCell = Cell startCellColor newStartCellPiece startCellColor   -- Create an empty cell with the same color
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

appEvent :: BrickEvent () e -> EventM () GameState ()
appEvent (VtyEvent e) = do
    gs <- get
    if myPlayer gs /= currentPlayer gs then do
        -- Access the connection object from the connection tuple and receive the data. We know that the data will be a valid move
        msg <- liftIO $ recv (connection gs) 1024
        newGameState <- liftIO $ executeMove gs (C.unpack msg)
        put newGameState
        return ()
    else do
      case e of
          V.EvKey V.KEsc [] -> halt
          V.EvKey (V.KChar c) [] -> do
              let newInput = userInput gs ++ [c]
              let parsedInput = if length newInput > 1 && isDigit (newInput !! 1) &&
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
              case parseMove (currentPlayer gs) moveInput of
                  Nothing -> put (gs { errorMsg = "Failed to parse move" })
                  Just (startPos, endPos) -> do
                      let (Just pc) = getPieceAt (board gs) startPos
                      newGameState <- if isLegalMove (board gs) startPos endPos
                                          then if compareColorPlayer (pc ^. pieceColor) (currentPlayer gs)
                                            then do
                                              liftIO $ executeMove gs moveInput
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
                            -- Send data to the other player
                            liftIO $ sendAll (connection gs) (C.pack moveInput)
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
                          -- Send data to the other player. It doesnt matter if we access connection of gs or newGameState since they are the same
                          liftIO $ sendAll (connection gs) (C.pack moveInput)
                          put newGameState { isCheck = isBool }

                      return ()
          _ -> return ()
appEvent _ = return ()

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
  -- Check if the port 8080 is already bound or not. If not then call the createServer which will make the current terminal the host
  -- If the port is already bound, then call the connectToServer which will make the current terminal the client
  isAvailable <- isPortAvailable "8080"
  if isAvailable then do
    putStrLn "Starting server..."
    -- Store the socket obtained from createServer in the first value of the connection tuple
    connection_object <- createServer
    let initGameState = initialGameState{connection = connection_object}
    _ <- defaultMain app initGameState
    return ()
  else do
    putStrLn "Connecting to server..."
    connection_object <- connectToServer
    let initGameState = initialGameState{myPlayer = Black, connection = connection_object}
    _ <- defaultMain app initGameState
    return ()