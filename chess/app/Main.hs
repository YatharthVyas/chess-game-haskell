{-# LANGUAGE TemplateHaskell #-}

module Main where
import Lens.Micro ((^.))
import Types
import TestData

{-# LANGUAGE OverloadedStrings #-}
import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (hCenter, vCenter)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V


data ChessSquare = Empty | Occupied PieceType
-- Function to render a chess piece
renderPiece :: PieceType -> Widget n
renderPiece piece = str $ case piece of
  King   -> "K"
  Queen  -> "Q"
  Rook   -> "R"
  Bishop -> "B"
  Knight -> "N"
  Pawn   -> "P"

-- Function to render a chess square
renderSquare :: Cell -> Widget n
renderSquare c = case c ^. cellPiece of
  Nothing        -> withAttr (attrName "empty") $ str " "
  Just pc  -> withAttr (attrName "occupied") $ renderPiece (pc ^. pieceType)

-- Function to render a chess board
renderBoard :: Board -> Widget n
renderBoard board =
  vBox $ map (hBox . map renderSquare) board

-- The app definition
app :: App Board e ()
app =
  App { appDraw = \s -> [renderBoard s]
      , appChooseCursor = neverShowCursor
      , appHandleEvent = \s e -> case e of
          VtyEvent (V.EvKey V.KEsc []) -> halt s
          _ -> continue s
      , appStartEvent = return
      , appAttrMap = const $ attrMap V.defAttr [(attrName "empty", V.white `on` V.black), (attrName "occupied", V.black `on` V.white)]
      }

main :: IO ()
main = do
  let ib = initialBoard
  _ <- defaultMain app ib
  return ()