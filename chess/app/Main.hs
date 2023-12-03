{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Redundant if" #-}

module Main where
import Lens.Micro ((^.))
import Types
import ValidateMove (parseMove)
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
import Brick (padTopBottom, padBottom, padTop)

-- Function to render a chess piece

renderPieceText :: String -> String -> String -> String
renderPieceText pColor blackText whiteText = if pColor == "blackPiece" then blackText else whiteText

padCell :: Widget n -> Widget n
padCell = padLeft (Pad 2) . padRight (Pad 3) . padTopBottom 1

renderPiece :: Maybe Piece -> V.Color -> Widget n
renderPiece piece c = case piece of
  Just a -> let colorAttr = if c == V.black then "blackCell" else "whiteCell"
                pColor = if a ^. pieceColor == V.black then "blackPiece" else "whitePiece"
              in  withAttr (attrName $ pColor ++ colorAttr) $ padCell $ str $ case a ^. pieceType of
                  King   -> renderPieceText pColor "♚" "♔"
                  Queen  -> renderPieceText pColor "♛" "♕"
                  Rook   -> renderPieceText pColor "♜" "♖"
                  Pawn   -> renderPieceText pColor "♟" "♙"
                  Bishop -> renderPieceText pColor "♝" "♗"
                  Knight -> renderPieceText pColor "♞" "♘"
  Nothing -> let colorAttr = if c == V.black then "blackSquare" else "lightSquare"
              in withAttr (attrName colorAttr) $ padCell $ str " "



-- Function to render a chess square
renderSquare :: Cell -> Widget n
renderSquare cell = case cell ^. cellPiece of
    Nothing  -> renderPiece Nothing (cell ^. cellColor)  -- Draw nothing for an empty cell
    Just pc  -> renderPiece (Just pc) (cell ^. cellColor)  -- Render the piece as before


-- Function to render a chess board
renderBoard :: Board -> Widget n
renderBoard board = vBox $ zipWith (\i row -> hBox $ padCell (str $ show (8 - i)) :
                  map renderSquare row) [0 .. ] (reverse board) ++
                  [hBox $ padCell (str "  ") : map (padCell . str . (: [])) ['a' .. 'h']]

initialGameState :: GameState
initialGameState = GameState initialBoard White "" ""

safeBack :: [a] -> [a]
safeBack [] = []
safeBack xs = init xs


makeMove :: Board -> (Int, Int) -> (Int, Int) -> Board
makeMove board (startRank, startFile) (endRank, endFile) =
  -- Write the code such that we extract the color of the cell of the end position and ensure that it doesnt change when we copy over the start cell
  -- This will ensure that the color of the cell is not changed when we move a piece
    let startCell = (board !! startRank) !! startFile
        endCell = (board !! endRank) !! endFile
        -- We want the updated position to have the piece from the start position and the color from the end position
        newStartCell = Cell (_cellColor endCell) (_cellPiece startCell)
        emptyCell = Cell (_cellColor startCell) Nothing  -- Create an empty cell with the same color
        updatedRowStart = take startFile (board !! startRank) ++ [emptyCell] ++ drop (startFile + 1) (board !! startRank)
        updatedBoardStart = take startRank board ++ [updatedRowStart] ++ drop (startRank + 1) board
        updatedRowEnd = take endFile (updatedBoardStart !! endRank) ++ [newStartCell] ++ drop (endFile + 1) (updatedBoardStart !! endRank)
        updatedBoardEnd = take endRank updatedBoardStart ++ [updatedRowEnd] ++ drop (endRank + 1) updatedBoardStart
    in updatedBoardEnd

executeMove :: GameState -> String -> IO GameState
executeMove gs moveInput =
  case parseMove (currentPlayer gs) moveInput of
    Just (startPos, endPos) ->
      if isLegalMove (board gs) startPos endPos then do
        putStrLn "Move executed."
        let newBoard = makeMove (board gs) startPos endPos
            move = userInput gs
            lastm = if length move == 4 then
                (move !! 0) : (move !! 1) : " -> " ++ (move !! 2) : [move !! 3]
            else ""
        return gs { board = newBoard, currentPlayer = if currentPlayer gs == White then Black else White, userInput = "", lastMove = lastm}
      else do
        putStrLn "Invalid move. Try again."
        return gs { userInput = "" }
    Nothing -> do
      putStrLn "Invalid move format."
      return gs { userInput = "" }

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
             put (gs { userInput = newInput })
             return ()
         -- backspace
         V.EvKey V.KBS [] -> do
             let newInput = safeBack (userInput gs)
             put (gs { userInput = newInput })
             return ()
         V.EvKey V.KEnter [] -> do
             -- Check if the move syntax is valid
             let moveInput = userInput gs

            --  let validSyntax = isValidChessMove moveInput

            --  -- the below lines are messing up the alignment of chessboard
            --  liftIO $ putStrLn $ if validSyntax then "Valid move syntax" else "Invalid move syntax"
             case parseMove (currentPlayer gs) moveInput of
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
                                            liftIO $ putStrLn "Not your turn!"
                                            return gs
                                    else do
                                      liftIO $ putStrLn "Invalid Move"
                                      return gs
                    put newGameState
                    return ()
         _ -> return ()
appEvent _ = return ()


gameTitle :: Widget n
gameTitle = padBottom (Pad 1) $ hCenter $ str "Press Esc to quit"

gameBigHeader :: Widget n
gameBigHeader = padTop (Pad 1) $ hCenter $ str " ██████╗██╗  ██╗███████╗███████╗███████╗\n██╔════╝██║  ██║██╔════╝██╔════╝██╔════╝\n██║     ███████║█████╗  ███████╗███████╗\n██║     ██╔══██║██╔══╝  ╚════██║╚════██║\n╚██████╗██║  ██║███████╗███████║███████║\n ╚═════╝╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝"

gameInstructions :: Widget n
gameInstructions = padLeft (Pad 3) $ hCenter $ str "Welcome to Chess Game!\nEnter a move in the format: e2e4 \nPress Enter to execute the move\nPress Esc to quit"

borderBottom :: Widget n
borderBottom = (<+> hBorder) $ str " "

borderLeft :: Widget n -> Widget n
borderLeft = (vBorder <+>)

app :: App GameState e ()
app =
  App {appDraw = \gs ->
            [ withBorderStyle unicodeRounded $ border $ vBox
                  [ gameBigHeader
                   , gameTitle
                   , borderBottom
                   , hBox [vCenter $ hCenter $ renderBoard (board gs), borderLeft $ vBox [ padTopBottom 2 gameInstructions
                    ,  borderBottom, padTop (Pad 2) $ padLeft (Pad 2) $ vBox [ (str $ "Current turn: " ++ show (currentPlayer gs))
                        , str "Enter your move: "
                        , str $ "Last Move: " ++ show (lastMove gs)
                        , str $ userInput gs
                      ]
                   ]
                ]
              ]
            ]
      , appChooseCursor = showFirstCursor
      , appHandleEvent = appEvent
      , appStartEvent = return ()
      , appAttrMap = const $ attrMap V.defAttr [ (attrName "blackPiecewhiteCell", V.black `on` V.rgbColor 227 193 111),
                                                  (attrName "whitePiecewhiteCell", V.white `on` V.rgbColor 227 193 111),
                                                  (attrName "blackPieceblackCell", V.black `on` V.rgbColor 184 139 74),
                                                  (attrName "whitePieceblackCell", V.white `on` V.rgbColor 184 139 74),
                                                  (attrName "lightSquare", V.rgbColor 220 220 220 `on` V.rgbColor 227 193 111),
                                                  (attrName "blackSquare", V.rgbColor 220 220 220 `on` V.rgbColor 184 139 74)
                                                ]
      }


main :: IO ()
main = do
  _ <- defaultMain app initialGameState
  return ()