{-# LANGUAGE TemplateHaskell #-}

module Main where
import Lens.Micro ((^.))
import Types
import TestData
import Data.Char (isDigit)
{-# LANGUAGE OverloadedStrings #-}
import Brick
import Brick.Widgets.Border (border,hBorder ,vBorder)
import Brick.Widgets.Center (hCenter, vCenter)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V
 -- Import liftIO
import Control.Monad.IO.Class (liftIO)

data ChessSquare = Empty | Occupied PieceType
-- Function to render a chess piece
renderPiece :: Maybe Piece -> Widget n
renderPiece piece = case piece of
  Just a -> let colorAttr = if a ^. pieceColor == V.black then "blackPiece" else "whitePiece"
              in  withAttr (attrName colorAttr) $ str $ case a ^. pieceType of
                King   -> "K"
                Queen  -> "Q"
                Rook   -> "R"
                Bishop -> "B"
                Knight -> "N"
                Pawn   -> "P"
  Nothing -> let colorAttr = "lightSquare" 
              in withAttr (attrName colorAttr) $ str $ " "
  


-- Function to render a chess square
renderSquare :: Cell -> Widget n
renderSquare cell = case cell ^. cellPiece of
    Nothing  -> renderPiece Nothing
    Just pc  -> renderPiece (Just pc)

-- Function to render a chess board
renderBoard :: Board -> Widget n
renderBoard board =
  vBox $ map (hBox . map renderSquare) board



data Player = White | Black deriving (Show, Eq)
data GameState = GameState { board :: Board, currentPlayer :: Player, userInput :: String }

initialGameState :: GameState
initialGameState = GameState initialBoard White ""

safeBack :: [a] -> [a]
safeBack [] = []
safeBack xs = init xs

isValidChessMove :: String -> Bool
isValidChessMove move
  | move == "O-O" || move == "O-O-O" = True  -- Castling
  | otherwise = case move of
      [file, rank] -> isFile file && isRank rank  -- Pawn move
      [piece, file, rank] -> isPiece piece && isFile file && isRank rank
      [file1, 'x', file2, rank] -> isFile file1 && isFile file2 && isRank rank  -- Pawn capture
      [piece, file, 'x', file2, rank] -> isPiece piece && isFile file && isFile file2 && isRank rank
      [piece, file1, rank1, file2, rank2] -> isPiece piece && isFile file1 && isRank rank1 && isFile file2 && isRank rank2  -- Disambiguating move
      [file, rank, '=', promPiece] -> isFile file && isRank rank && isPromotionPiece promPiece  -- Pawn promotion
      _ -> False
  where
    isFile f = f `elem` ['a'..'h']
    isRank r = r `elem` ['1'..'8']
    isPiece p = p `elem` "KQRBN"
    isPromotionPiece p = p `elem` "QRBN"



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
      , appHandleEvent = \gs e -> case e of
          VtyEvent (V.EvKey V.KEsc []) -> halt gs
          VtyEvent (V.EvKey (V.KChar c) []) -> continue $ gs { userInput = userInput gs ++ [c] }
          VtyEvent (V.EvKey V.KEnter []) -> do
              -- Check if the move itself is valid
              let validMove = isValidChessMove (userInput gs)
              liftIO $ putStrLn $ if validMove then "Valid move" else "Invalid move"
              liftIO $ putStrLn ("User input: " ++ userInput gs)  -- Debugging output
              -- Implement the logic to actually change the board state if the move is valid
              
              -- Change player only if the move is valid
              let newGameState = if validMove 
                          then gs {currentPlayer = if currentPlayer gs == White then Black else White, userInput = "" } 
                            else gs { userInput = "" }
              continue newGameState

          VtyEvent (V.EvKey V.KBS []) -> continue $ gs { userInput = safeBack (userInput gs) }
          VtyEvent (V.EvKey (V.KChar 'm') []) -> captureMove gs -- 'm' for move, this is a placeholder
          _ -> continue gs
      , appStartEvent = return
      , appAttrMap = const $ attrMap V.defAttr [ (attrName "blackPiece", V.white `on` V.black)
                                                , (attrName "whitePiece", V.black `on` V.white)
                                                , (attrName "lightSquare", (V.rgbColor 220 220 220) `on` V.white) -- Cant directly use V.rgbColor in attrMap
                                                , (attrName "darkSquare", V.black `on` V.green)
                                                ]
      }
captureMove :: GameState -> EventM n (Next GameState)
captureMove gs = do

  -- Here, you would implement the logic to capture and process the move
  continue gs  -- Return the updated GameState


main :: IO ()
main = do
  let initialState = GameState initialBoard White ""
  _ <- defaultMain app initialState
  return ()