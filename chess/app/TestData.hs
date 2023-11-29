module TestData where

import Types


generateCell :: Color -> Maybe Piece -> Cell
generateCell c p = Cell c p

generatePiece :: Color -> PieceType -> (Int, Int) -> Piece
generatePiece c pt (x, y) = Piece c pt (x, y)

EmptyBlackCell = generateCell Black Nothing
EmptyWhiteCell = generateCell White Nothing

BlackPawn = generatePiece Black Pawn
WhitePawn = generatePiece White Pawn


-- generate the initial layout of a chess board
initialBoard :: Board
initialBoard =