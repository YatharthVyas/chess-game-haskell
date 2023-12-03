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

renderPieceText :: String -> String -> String -> String
renderPieceText pColor blackText whiteText = if pColor == "blackPiece" then blackText else whiteText

padCell :: Widget n -> Widget n
padCell = padLeft (Pad 2) . padRight (Pad 3) . padTopBottom 1

-- Function to render a chess piece
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


attributeMap = const $ attrMap V.defAttr [ (attrName "blackPiecewhiteCell", V.black `on` V.rgbColor 227 193 111),
                                                  (attrName "whitePiecewhiteCell", V.white `on` V.rgbColor 227 193 111),
                                                  (attrName "blackPieceblackCell", V.black `on` V.rgbColor 184 139 74),
                                                  (attrName "whitePieceblackCell", V.white `on` V.rgbColor 184 139 74),
                                                  (attrName "lightSquare", V.rgbColor 220 220 220 `on` V.rgbColor 227 193 111),
                                                  (attrName "blackSquare", V.rgbColor 220 220 220 `on` V.rgbColor 184 139 74)
                                                ]

drawUI :: GameState -> [Widget ()]
drawUI gs = [ withBorderStyle unicodeRounded $ border $ vBox
                  [ gameBigHeader
                   , gameTitle
                   , borderBottom
                   , hBox [vCenter $ hCenter $ renderBoard (board gs), borderLeft $ vBox [ padTopBottom 2 gameInstructions
                    ,  borderBottom, padTop (Pad 2) $ padLeft (Pad 2) $ vBox [ (str $ "Current turn: " ++ show (currentPlayer gs))
                        , str $ "Last Move: " ++ show (lastMove gs)
                        , str $ "Log: " ++ show (errorMsg gs)
                        , str "Enter your move: "
                        , str $ userInput gs
                      ]
                   ]
                ]
              ]
            ]