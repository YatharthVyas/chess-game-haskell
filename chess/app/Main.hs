{-# LANGUAGE TemplateHaskell #-}

module Main where
import Lens.Micro ((^.))
import Types
import ValidateMove (isValidChessMove, parseMove)
import Piece (isLegalMove, getPieceAt, canMove, pathClear)
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

data ChessSquare = Empty | Occupied PieceType
-- Function to render a chess piece
renderPiece :: Maybe Piece -> V.Color -> Widget n
renderPiece piece c = case piece of
  Just a -> let colorAttr = if c == V.black then "blackPiece" else "whitePiece"
                pColor = a ^. pieceColor
              in  withAttr (attrName colorAttr) $ str $ case a ^. pieceType of
                  King   -> if pColor == V.black then " ♚ " else " ♔ "
                  Queen  -> if pColor == V.black then " ♛ " else " ♕ "
                  Rook   -> if pColor == V.black then " ♜ " else " ♖ "
                  Pawn   -> if pColor == V.black then " ♟ " else " ♙ "
                  Bishop -> if pColor == V.black then " ♝ " else " ♗ "
                  Knight -> if pColor == V.black then " ♞ " else " ♘ "
  Nothing -> let colorAttr = if c == V.black then "blackSquare" else "lightSquare"
              in withAttr (attrName colorAttr) $ str $ "   "



-- Function to render a chess square
renderSquare :: Cell -> Widget n
renderSquare cell = case cell ^. cellPiece of
    Nothing  -> renderPiece Nothing (cell ^. cellColor)  -- Draw nothing for an empty cell
    Just pc  -> renderPiece (Just pc) (cell ^. cellColor)  -- Render the piece as before


-- Function to render a chess board
renderBoard :: Board -> Widget n
renderBoard board =
  vBox $ map (hBox . map renderSquare) board

initialGameState :: GameState
initialGameState = GameState initialBoard White ""

safeBack :: [a] -> [a]
safeBack [] = []
safeBack xs = init xs


makeMove :: Board -> (Int, Int) -> (Int, Int) -> Board
makeMove board (startRank, startFile) (endRank, endFile) =
    let startCell = (board !! startRank) !! startFile
        emptyCell = Cell (_cellColor startCell) Nothing  -- Create an empty cell with the same color
        updatedRowStart = take startFile (board !! startRank) ++ [emptyCell] ++ drop (startFile + 1) (board !! startRank)
        updatedBoardStart = take startRank board ++ [updatedRowStart] ++ drop (startRank + 1) board
        updatedRowEnd = take endFile (updatedBoardStart !! endRank) ++ [startCell] ++ drop (endFile + 1) (updatedBoardStart !! endRank)
        updatedBoardEnd = take endRank updatedBoardStart ++ [updatedRowEnd] ++ drop (endRank + 1) updatedBoardStart
    in updatedBoardEnd

executeMove :: GameState -> String -> IO GameState
executeMove gs moveInput =
  case parseMove (currentPlayer gs) moveInput of
    Just (startPos, endPos) ->
      if isLegalMove (board gs) startPos endPos then do
        putStrLn "Move executed."
        let newBoard = makeMove (board gs) startPos endPos
        return gs { board = newBoard, currentPlayer = if currentPlayer gs == White then Black else White, userInput = "" }
      else do
        putStrLn "Invalid move. Try again."
        return gs { userInput = "" }
    Nothing -> do
      putStrLn "Invalid move format."
      return gs { userInput = "" }

appEvent :: BrickEvent () e -> EventM () GameState ()
appEvent (VtyEvent e) = do
    gs <- get
    case e of
         V.EvKey (V.KChar 'q') [] -> halt
         V.EvKey (V.KEsc) [] -> halt
        --  V.EvKey (V.KChar c) [] -> return gs
         _ -> return ()
appEvent _ = return ()

-- \gs e -> case e of
        --   VtyEvent (V.EvKey V.KEsc []) -> halt gs
        --   VtyEvent (V.EvKey (V.KChar c) []) -> continue $ gs { userInput = userInput gs ++ [c] }
        --   VtyEvent (V.EvKey V.KEnter []) -> do
        --       -- Check if the move syntax is valid
        --       let moveInput = userInput gs
        --       let validSyntax = isValidChessMove moveInput
        --       liftIO $ putStrLn $ if validSyntax then "Valid move syntax" else "Invalid move syntax"
        --       case parseMove gs moveInput of
        --           Just (startPos, endPos) -> do
        --               liftIO $ putStrLn $ "Start position: " ++ show startPos
        --               liftIO $ putStrLn $ "End position: " ++ show endPos
        --           Nothing -> liftIO $ putStrLn "Failed to parse move"

        --       -- Execute the move if the syntax is valid
        --       newGameState <- if validSyntax
        --                       then liftIO $ executeMove gs moveInput
        --                       else return gs
        --       continue newGameState
        --   VtyEvent (V.EvKey V.KBS []) -> continue $ gs { userInput = safeBack (userInput gs) }
        --   _ -> continue gs

-- The app definition
app :: App GameState e ()
app =
  App {appDraw = \gs ->
            [ vBox [ renderBoard (board gs)
                   , padLeft (Pad 2) (str $ "Current turn: " ++ show (currentPlayer gs))
                   , padLeft (Pad 2) (str "Enter your move: ")
                   , padLeft (Pad 2) (str $ userInput gs)
                   ]
            ]
      , appChooseCursor = showFirstCursor
      , appHandleEvent = appEvent
      , appStartEvent = return ()
      , appAttrMap = const $ attrMap V.defAttr [ (attrName "blackPiece", V.white `on` V.black)
                                                , (attrName "whitePiece", V.black `on` V.white)
                                                , (attrName "lightSquare", (V.rgbColor 220 220 220) `on` V.white) -- Cant directly use V.rgbColor in attrMap
                                                ]
      }


main :: IO ()
main = do
  _ <- defaultMain app initialGameState
  return ()