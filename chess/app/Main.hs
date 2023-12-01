{-# LANGUAGE TemplateHaskell #-}

module Main where
import Lens.Micro ((^.))
import Types
import Piece (isLegalMove, getPieceAt)
import TestData
import Data.Char (isDigit, isUpper)
{-# LANGUAGE OverloadedStrings #-}
import Brick
import Brick.Widgets.Border.Style
import Brick.Widgets.Border (border,hBorder ,vBorder, borderWithLabel)
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
    Nothing  -> renderPiece Nothing  -- Draw nothing for an empty cell
    Just pc  -> renderPiece (Just pc)  -- Render the piece as before


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

{-
Algebraic notation for chess moves and their validation
Cases covered:
Captures (e.g., exd5).
Pawn promotions (e.g., e8=Q).
Disambiguating moves where two (or more) identical pieces can move to the same square (e.g., Nbd2, Rae1).
Special moves like castling (O-O for kingside and O-O-O for queenside).
Check (+) and checkmate (#) symbols.
-}
isValidChessMove :: String -> Bool
isValidChessMove move
  | move `elem` ["O-O", "O-O-O"] = True  -- Castling
  | otherwise = case removeCheckCheckmate move of
      [file, rank] -> isFile file && isRank rank  -- Single-step pawn move
      [file1, rank1, file2, rank2] -> isFile file1 && isRank rank1 && isFile file2 && isRank rank2 && file1 == file2 && isPawnDoubleStep rank1 rank2  -- Double-step pawn move
      [piece, file, rank] -> isPiece piece && isFile file && isRank rank
      [file1, 'x', file2, rank] -> isFile file1 && isFile file2 && isRank rank  -- Pawn capture
      [piece, file, 'x', file2, rank] -> isPiece piece && isFile file && isFile file2 && isRank rank
      [piece, file1, rank1, file2, rank2] -> isPiece piece && isFile file1 && isRank rank1 && isFile file2 && isRank rank2  -- Disambiguating move
      [file, rank, '=', promPiece] -> isFile file && isRank rank && isPromotionPiece promPiece  -- Pawn promotion
      _ -> False
  where
    removeCheckCheckmate s = filter (`notElem` ['+', '#']) s 
    isFile f = f `elem` ['a'..'h']
    isRank r = r `elem` ['1'..'8']
    isPiece p = isUpper p && p `elem` "KQRBN"
    isPromotionPiece p = p `elem` "QRBN"
    isPawnDoubleStep r1 r2 = (r1 == '2' && r2 == '4') || (r1 == '7' && r2 == '5')


-- Now that a move is valid, we need to write functions which take the Gamestate and the move and update the board if the move is valid

fileToIndex :: Char -> Int
fileToIndex file = fromEnum file - fromEnum 'a'

rankToIndex :: GameState -> Char -> Int
rankToIndex gs rank = if currentPlayer gs == White then 7 - (fromEnum rank - fromEnum '1') else fromEnum rank - fromEnum '1'   -- Subtract from 8 for zero-indexing

-- Parses a move string (e.g., "e2e4", "e4", "Nf3") into start and end board indices
parseMove :: GameState -> String -> Maybe ((Int, Int), (Int, Int))
parseMove gs move
  | length move == 4 = 
        let startFile = fileToIndex (move !! 0)
            startRank = rankToIndex gs (move !! 1)
            endFile = fileToIndex (move !! 2)
            endRank = rankToIndex gs (move !! 3)
        in Just ((startRank, startFile), (endRank, endFile))
  | length move == 2 && isPawnMove move =  -- Handle pawn move (e.g., "e4")
      let startFile = fileToIndex (move !! 0)
          startRank = rankToIndex gs (move !! 1)
          endFile = fileToIndex (move !! 0)  -- The end file is the same as the start file for pawn moves
          endRank = rankToIndex gs (move !! 1) + if currentPlayer gs == White then 1 else -1  -- Adjust rank for pawn move
      in Just ((startRank, startFile), (endRank, endFile))
  | length move == 3 && isKnightMove move =  -- Handle knight move (e.g., "Nf3")
      let startFile = fileToIndex (move !! 1)
          startRank = rankToIndex gs (move !! 2)
          (deltaRank, deltaFile) = case move !! 0 of
            'N' -> (1, 2)  -- Knight moves have a fixed relative offset
            _ -> (-1, -2)
          endFile = startFile + deltaFile
          endRank = startRank + deltaRank
      in if isValidBoardPosition endRank endFile then Just ((startRank, startFile), (endRank, endFile)) else Nothing
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
  case parseMove gs moveInput of
    Just (startPos, endPos) -> 
      if isLegalMove (board gs) startPos endPos then do
        putStrLn "Move executed."
        let newBoard = makeMove (board gs) startPos endPos
        return gs { board = newBoard, currentPlayer = if currentPlayer gs == White then Black else White, userInput = "" }
      else do
        putStrLn "Invalid move. Try again."
        return gs
    Nothing -> do
      putStrLn "Invalid move format."
      return gs

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
              -- Check if the move syntax is valid
              let moveInput = userInput gs
              let validSyntax = isValidChessMove moveInput
              liftIO $ putStrLn $ if validSyntax then "Valid move syntax" else "Invalid move syntax"
              liftIO $ putStrLn ("User input: " ++ moveInput)  -- Debugging output
              case parseMove gs moveInput of
                  Just (startPos, endPos) -> do
                      liftIO $ putStrLn $ "Start position: " ++ show startPos
                      liftIO $ putStrLn $ "End position: " ++ show endPos
                  Nothing -> liftIO $ putStrLn "Failed to parse move"
              -- Execute the move if the syntax is valid
              newGameState <- if validSyntax
                              then liftIO $ executeMove gs moveInput
                              else return gs
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