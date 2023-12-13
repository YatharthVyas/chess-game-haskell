module TestData where

import Graphics.Vty.Attributes
import Types

blackPawn :: Piece
blackPawn = Piece black Pawn

whitePawn :: Piece
whitePawn = Piece white Pawn

blackKnight :: Piece
blackKnight = Piece black Knight

whiteKnight :: Piece
whiteKnight = Piece white Knight

blackBishop :: Piece
blackBishop = Piece black Bishop

whiteBishop :: Piece
whiteBishop = Piece white Bishop

blackRook :: Piece
blackRook = Piece black Rook

whiteRook :: Piece
whiteRook = Piece white Rook

blackQueen :: Piece
blackQueen = Piece black Queen

whiteQueen :: Piece
whiteQueen = Piece white Queen

blackKing :: Piece
blackKing = Piece black King

whiteKing :: Piece
whiteKing = Piece white King

-- generate a row of cells
generateRow :: Int -> [Maybe Piece] -> [Cell]
generateRow y [p1, p2, p3, p4, p5, p6, p7, p8] = [
                                                    Cell c1 p1 c1,
                                                    Cell c2 p2 c2,
                                                    Cell c1 p3 c1,
                                                    Cell c2 p4 c2,
                                                    Cell c1 p5 c1,
                                                    Cell c2 p6 c2,
                                                    Cell c1 p7 c1,
                                                    Cell c2 p8 c2
                                                ]
                                                where
                                                    c1 = if y `mod` 2 == 1 then white else black
                                                    c2 = if y `mod` 2 == 1 then black else white

-- initial layout of a chess board
initialBoard :: Board
initialBoard = [generateRow 0 [Just whiteRook, Just whiteKnight, Just whiteBishop, Just whiteQueen, Just whiteKing, Just whiteBishop, Just whiteKnight, Just whiteRook],
                generateRow 1 (replicate 8 (Just whitePawn)),
                generateRow 2 (replicate 8 Nothing),
                generateRow 3 (replicate 8 Nothing),
                generateRow 4 (replicate 8 Nothing),
                generateRow 5 (replicate 8 Nothing),
                generateRow 6 (replicate 8 (Just blackPawn)),
                generateRow 7 [Just blackRook, Just blackKnight, Just blackBishop, Just blackQueen, Just blackKing, Just blackBishop, Just blackKnight, Just blackRook]
                ]

-- checkmate layout of a chess board
-- initialBoard :: Board
-- initialBoard = [generateRow 0 [Just whiteRook, Just whiteKnight, Just whiteBishop, Just whiteQueen, Just whiteKing, Just whiteBishop, Just whiteKnight, Just whiteRook],
--                 generateRow 1 [Just whitePawn, Just whitePawn, Just whitePawn, Nothing, Just whitePawn, Just whitePawn, Just whitePawn, Just whitePawn],
--                 generateRow 2 (replicate 8 Nothing),
--                 generateRow 3 [Nothing, Nothing, Nothing, Just whitePawn, Nothing, Nothing, Nothing, Nothing], 
--                 generateRow 4 [Nothing, Nothing, Just blackBishop, Nothing, Nothing, Nothing, Nothing, Nothing], 
--                 generateRow 5 (replicate 8 Nothing),
--                 generateRow 6 [Just blackPawn, Just blackPawn, Just blackPawn, Just blackPawn, Nothing, Just blackPawn, Just blackPawn, Just blackPawn],
--                 generateRow 7 [Just blackRook, Just blackKnight, Just blackBishop, Just blackQueen, Just blackKing, Nothing, Just blackKnight, Just blackRook]
--                 ]

-- initial layout of a chess board without pawns for testing
initialBoardWithoutPawn :: Board
initialBoardWithoutPawn = [generateRow 0 [Just whiteRook, Just whiteKnight, Just whiteBishop, Just whiteQueen, Just whiteKing, Just whiteBishop, Just whiteKnight, Just whiteRook],
                generateRow 1 (replicate 8 Nothing),
                generateRow 2 (replicate 8 Nothing),
                generateRow 3 (replicate 8 Nothing),
                generateRow 4 (replicate 8 Nothing),
                generateRow 5 (replicate 8 Nothing),
                generateRow 6 (replicate 8 Nothing),
                generateRow 7 [Just blackRook, Just blackKnight, Just blackBishop, Just blackQueen, Just blackKing, Just blackBishop, Just blackKnight, Just blackRook]
                ]

---------------------------------------------
------  Test Data for Piece Movement  -------
---------------------------------------------

-- >>> replicate 3 emptyCell
-- [Cell {_cellColor = ISOColor 7, _cellPiece = Nothing},Cell {_cellColor = ISOColor 7, _cellPiece = Nothing},Cell {_cellColor = ISOColor 7, _cellPiece = Nothing}]

-- >>> initialBoard
-- [[Cell {_cellColor = ISOColor 0, _cellPiece = Just (Piece {_pieceColor = ISOColor 0, _pieceType = Rook})},Cell {_cellColor = ISOColor 7, _cellPiece = Just (Piece {_pieceColor = ISOColor 0, _pieceType = Knight})},Cell {_cellColor = ISOColor 0, _cellPiece = Just (Piece {_pieceColor = ISOColor 0, _pieceType = Bishop})},Cell {_cellColor = ISOColor 7, _cellPiece = Just (Piece {_pieceColor = ISOColor 0, _pieceType = Queen})},Cell {_cellColor = ISOColor 0, _cellPiece = Just (Piece {_pieceColor = ISOColor 0, _pieceType = King})},Cell {_cellColor = ISOColor 7, _cellPiece = Just (Piece {_pieceColor = ISOColor 0, _pieceType = Bishop})},Cell {_cellColor = ISOColor 0, _cellPiece = Just (Piece {_pieceColor = ISOColor 0, _pieceType = Knight})},Cell {_cellColor = ISOColor 7, _cellPiece = Just (Piece {_pieceColor = ISOColor 0, _pieceType = Rook})}],[Cell {_cellColor = ISOColor 7, _cellPiece = Just (Piece {_pieceColor = ISOColor 0, _pieceType = Pawn})},Cell {_cellColor = ISOColor 0, _cellPiece = Just (Piece {_pieceColor = ISOColor 0, _pieceType = Pawn})},Cell {_cellColor = ISOColor 7, _cellPiece = Just (Piece {_pieceColor = ISOColor 0, _pieceType = Pawn})},Cell {_cellColor = ISOColor 0, _cellPiece = Just (Piece {_pieceColor = ISOColor 0, _pieceType = Pawn})},Cell {_cellColor = ISOColor 7, _cellPiece = Just (Piece {_pieceColor = ISOColor 0, _pieceType = Pawn})},Cell {_cellColor = ISOColor 0, _cellPiece = Just (Piece {_pieceColor = ISOColor 0, _pieceType = Pawn})},Cell {_cellColor = ISOColor 7, _cellPiece = Just (Piece {_pieceColor = ISOColor 0, _pieceType = Pawn})},Cell {_cellColor = ISOColor 0, _cellPiece = Just (Piece {_pieceColor = ISOColor 0, _pieceType = Pawn})}],[Cell {_cellColor = ISOColor 0, _cellPiece = Nothing},Cell {_cellColor = ISOColor 7, _cellPiece = Nothing},Cell {_cellColor = ISOColor 0, _cellPiece = Nothing},Cell {_cellColor = ISOColor 7, _cellPiece = Nothing},Cell {_cellColor = ISOColor 0, _cellPiece = Nothing},Cell {_cellColor = ISOColor 7, _cellPiece = Nothing},Cell {_cellColor = ISOColor 0, _cellPiece = Nothing},Cell {_cellColor = ISOColor 7, _cellPiece = Nothing}],[Cell {_cellColor = ISOColor 7, _cellPiece = Nothing},Cell {_cellColor = ISOColor 0, _cellPiece = Nothing},Cell {_cellColor = ISOColor 7, _cellPiece = Nothing},Cell {_cellColor = ISOColor 0, _cellPiece = Nothing},Cell {_cellColor = ISOColor 7, _cellPiece = Nothing},Cell {_cellColor = ISOColor 0, _cellPiece = Nothing},Cell {_cellColor = ISOColor 7, _cellPiece = Nothing},Cell {_cellColor = ISOColor 0, _cellPiece = Nothing}],[Cell {_cellColor = ISOColor 0, _cellPiece = Nothing},Cell {_cellColor = ISOColor 7, _cellPiece = Nothing},Cell {_cellColor = ISOColor 0, _cellPiece = Nothing},Cell {_cellColor = ISOColor 7, _cellPiece = Nothing},Cell {_cellColor = ISOColor 0, _cellPiece = Nothing},Cell {_cellColor = ISOColor 7, _cellPiece = Nothing},Cell {_cellColor = ISOColor 0, _cellPiece = Nothing},Cell {_cellColor = ISOColor 7, _cellPiece = Nothing}],[Cell {_cellColor = ISOColor 7, _cellPiece = Nothing},Cell {_cellColor = ISOColor 0, _cellPiece = Nothing},Cell {_cellColor = ISOColor 7, _cellPiece = Nothing},Cell {_cellColor = ISOColor 0, _cellPiece = Nothing},Cell {_cellColor = ISOColor 7, _cellPiece = Nothing},Cell {_cellColor = ISOColor 0, _cellPiece = Nothing},Cell {_cellColor = ISOColor 7, _cellPiece = Nothing},Cell {_cellColor = ISOColor 0, _cellPiece = Nothing}],[Cell {_cellColor = ISOColor 0, _cellPiece = Just (Piece {_pieceColor = ISOColor 7, _pieceType = Pawn})},Cell {_cellColor = ISOColor 7, _cellPiece = Just (Piece {_pieceColor = ISOColor 7, _pieceType = Pawn})},Cell {_cellColor = ISOColor 0, _cellPiece = Just (Piece {_pieceColor = ISOColor 7, _pieceType = Pawn})},Cell {_cellColor = ISOColor 7, _cellPiece = Just (Piece {_pieceColor = ISOColor 7, _pieceType = Pawn})},Cell {_cellColor = ISOColor 0, _cellPiece = Just (Piece {_pieceColor = ISOColor 7, _pieceType = Pawn})},Cell {_cellColor = ISOColor 7, _cellPiece = Just (Piece {_pieceColor = ISOColor 7, _pieceType = Pawn})},Cell {_cellColor = ISOColor 0, _cellPiece = Just (Piece {_pieceColor = ISOColor 7, _pieceType = Pawn})},Cell {_cellColor = ISOColor 7, _cellPiece = Just (Piece {_pieceColor = ISOColor 7, _pieceType = Pawn})}],[Cell {_cellColor = ISOColor 7, _cellPiece = Just (Piece {_pieceColor = ISOColor 7, _pieceType = Rook})},Cell {_cellColor = ISOColor 0, _cellPiece = Just (Piece {_pieceColor = ISOColor 7, _pieceType = Knight})},Cell {_cellColor = ISOColor 7, _cellPiece = Just (Piece {_pieceColor = ISOColor 7, _pieceType = Bishop})},Cell {_cellColor = ISOColor 0, _cellPiece = Just (Piece {_pieceColor = ISOColor 7, _pieceType = Queen})},Cell {_cellColor = ISOColor 7, _cellPiece = Just (Piece {_pieceColor = ISOColor 7, _pieceType = King})},Cell {_cellColor = ISOColor 0, _cellPiece = Just (Piece {_pieceColor = ISOColor 7, _pieceType = Bishop})},Cell {_cellColor = ISOColor 7, _cellPiece = Just (Piece {_pieceColor = ISOColor 7, _pieceType = Knight})},Cell {_cellColor = ISOColor 0, _cellPiece = Just (Piece {_pieceColor = ISOColor 7, _pieceType = Rook})}]]