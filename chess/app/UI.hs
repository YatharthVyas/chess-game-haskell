module UI where

import Brick
import Brick.Main
import Brick.Widgets.Core
import Brick.Widgets.Border.Style
import Brick.Widgets.Border (border,hBorder ,vBorder, borderWithLabel)
import Brick.Widgets.Center (hCenter, vCenter)
import Control.Monad.State
import Data.Monoid ((<>))
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Types
import Graphics.Vty (Color)

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

renderPieceText :: V.Color -> String -> String -> String
renderPieceText pColor blackText whiteText = if pColor == V.black then blackText else whiteText

padCell :: Widget n -> Widget n
padCell = padLeft (Pad 2) . padRight (Pad 3) . padTopBottom 1

getAttributePiece:: Color -> Color -> Color -> String
getAttributePiece pieceColor cellColor borderColor = let piece = if pieceColor == V.black then "BlackPiece" else "WhitePiece"
                                                         cell = if cellColor == V.black then "BlackCell" else "WhiteCell"
                                                         in
                                                         if cellColor /= borderColor then "PossibleMove" ++ piece else piece ++ cell

getAttributeCell:: Color -> Color -> String
getAttributeCell cellColor borderColor = let cell = if cellColor == V.black then "BlackCell" else "WhiteCell"
                                              in
                                              if cellColor /= borderColor then "PossibleMove" ++ cell else cell

-- Function to render a chess piece
renderPiece :: Maybe Piece -> V.Color -> V.Color -> Widget n
renderPiece piece cellColor borderColor = case piece of
  Just a -> let pColor = a ^. pieceColor
              in  withAttr (attrName $ getAttributePiece pColor cellColor borderColor) $ padCell $ str $ case a ^. pieceType of
                  King   -> renderPieceText pColor "♚" "♔"
                  Queen  -> renderPieceText pColor "♛" "♕"
                  Rook   -> renderPieceText pColor "♜" "♖"
                  Pawn   -> renderPieceText pColor "♟" "♙"
                  Bishop -> renderPieceText pColor "♝" "♗"
                  Knight -> renderPieceText pColor "♞" "♘"
  Nothing -> withAttr (attrName $ getAttributeCell cellColor borderColor) $ padCell $ str $ if cellColor == borderColor then " " else "●"

-- Function to render a chess square
renderSquare :: Cell -> Widget n
renderSquare cell = case cell ^. cellPiece of
    Nothing  -> renderPiece Nothing (cell ^. cellColor) (cell ^. borderColor) -- Draw nothing for an empty cell
    Just pc  -> renderPiece (Just pc) (cell ^. cellColor) (cell ^. borderColor)  -- Render the piece as before


-- Function to render a chess board
renderBoard :: Board -> Widget n
renderBoard board = vBox $ zipWith (\i row -> hBox $ padCell (str $ show (8 - i)) :
                  map renderSquare row) [0 .. ] (reverse board) ++
                  [hBox $ padCell (str "  ") : map (padCell . str . (: [])) ['a' .. 'h']]


attributeMap = const $ attrMap V.defAttr [ (attrName "BlackPiecewhiteCell", V.black `on` V.rgbColor 227 193 111),
                                                  (attrName "WhitePieceWhiteCell", V.white `on` V.rgbColor 227 193 111),
                                                  (attrName "BlackPieceBlackCell", V.black `on` V.rgbColor 184 139 74),
                                                  (attrName "WhitePieceBlackCell", V.white `on` V.rgbColor 184 139 74),
                                                  (attrName "BlackPieceWhiteCell", V.black `on` V.rgbColor 227 193 111),
                                                  (attrName "WhiteCell", V.rgbColor 227 193 111 `on` V.rgbColor 227 193 111), -- rgb(227, 193, 111)
                                                  (attrName "BlackCell", V.rgbColor 184 139 74 `on` V.rgbColor 184 139 74),  -- rgb(184, 139, 74)
                                                  (attrName "startBlackPiece", V.black `on` V.rgbColor 87 143 96),              -- rgb(87, 143, 96)
                                                  (attrName "startWhitePiece", V.white `on` V.rgbColor 87 143 96),
                                                  (attrName "PossibleMoveWhiteCell", V.rgbColor 171 202 60 `on` V.rgbColor 227 193 111), -- rgb(171, 202, 60)
                                                  (attrName "PossibleMoveBlackCell", V.rgbColor 171 202 60 `on` V.rgbColor 184 139 74),
                                                  (attrName "PossibleMoveWhitePiece", V.white `on` V.rgbColor 180 80 80),      -- rgb(180, 80, 80)
                                                  (attrName "PossibleMoveBlackPiece", V.black `on` V.rgbColor 180 80 80)
                                                  -- (attrName "border", V.rgbColor 220 220 220 `on` V.rgbColor 184 139 74)
                                                ]

drawUI :: GameState -> [Widget ()]
drawUI gs = [ withBorderStyle unicodeRounded $ border $ vBox
                  [ gameBigHeader
                   , gameTitle
                   , borderBottom
                   , hBox [vCenter $ hCenter $ renderBoard (board gs), borderLeft $ vBox [ padTopBottom 2 gameInstructions
                    ,  borderBottom, padTop (Pad 2) $ padLeft (Pad 2) $ vBox [ str $ "Current turn: " ++ show (currentPlayer gs)
                        , str $ "Last Move: " ++ show (lastMove gs)
                        , str $ "Log: " ++ show (errorMsg gs)
                        , str $ "Check: " ++ show (isCheck gs)
                        , str "Enter your move: "
                        , str $ userInput gs
                      ]
                   ]
                ]
              ]
            ]


-- Replaces the borderColor of cellPiece at Int,Int with blue
highlightStartCell :: Board -> (Int, Int) -> V.Color -> Board
highlightStartCell board (startRank, startFile) c =
    let startCell = (board !! startRank) !! startFile
        startCellColor = _cellColor startCell
        newStartCell = Cell startCellColor (_cellPiece startCell) c
        updatedRowStart = take startFile (board !! startRank) ++ [newStartCell] ++ drop (startFile + 1) (board !! startRank)
        updatedBoardStart = take startRank board ++ [updatedRowStart] ++ drop (startRank + 1) board
    in updatedBoardStart

highlightPossibleMoves :: Board -> [(Int, Int)] -> Board
highlightPossibleMoves board possibleMovesList =
    let c = V.rgbColor 0 0 255
        highlightedBoard = foldl (\b (rank, file) -> highlightStartCell b (rank, file) c) board possibleMovesList
    in highlightedBoard

-- resets the borderColor of each cell to it's cellColor
resetHighlightedBoard :: Board -> Board
resetHighlightedBoard board =
    let resetCell cell = Cell (cell ^. cellColor) (cell ^. cellPiece) (cell ^. cellColor)
        resetRow = map resetCell
        resetBoard = map resetRow board
    in resetBoard
