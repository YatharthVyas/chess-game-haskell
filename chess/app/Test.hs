{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit
import TestData 
import Lens.Micro
import Piece
import Types
import Graphics.Vty.Attributes
import Control.Exception (assert)
import Check (isKingCheck, isCheckMate)
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [movesTests, boardTests, pieceTests, checkTest, checkMateTest]

movesTests :: TestTree
movesTests = testGroup "Piece Moves"
  [ testCase "Initial Board Horse Possible Moves" $
      assertEqual "Horse can jump over other pieces and move to 2 possible places" (getPossibleMoves initialBoard (0,1)) [(2,0),(2,2)]
  , testCase "Initial Board Pawn Possible Moves" $
      assertEqual "Pawn can move either 1 step or 2 step forward" (getPossibleMoves initialBoard (1,1)) [(2,1),(3,1)]
  , testCase "Initial Board without Pawn - Rook" $
      assertEqual "Rook can move in X dir till it encounters enemy pawn" (getPossibleMoves initialBoardWithoutPawn (0,0)) [(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0)]
  , testCase "Initial Board without Pawn - Knight" $
      assertEqual "Knight can move in L shape" (getPossibleMoves initialBoardWithoutPawn (0,1)) [(1,3),(2,0),(2,2)]
  , testCase "Initial Board without Pawn - Bishop" $
      assertEqual "Bishop can move in diagonal dir till it encounters enemy pawn" (getPossibleMoves initialBoardWithoutPawn (0,2)) [(1,1),(1,3),(2,0),(2,4),(3,5),(4,6),(5,7)]
  , testCase "Initial Board without Pawn - Queen" $
      assertEqual "Queen can move in X or diagonal direction" (getPossibleMoves initialBoardWithoutPawn (0,3)) [(1,2),(1,3),(1,4),(2,1),(2,3),(2,5),(3,0),(3,3),(3,6),(4,3),(4,7),(5,3),(6,3),(7,3)]
  , testCase "Initial Board without Pawn - King" $
      assertEqual "King can move one step in any direction" (getPossibleMoves initialBoardWithoutPawn (0,4)) [(1,3),(1,4),(1,5)]
  , testCase "Invalid Move Pawn - Diagonal without takedown" $
      assertEqual "Pawn cannot move diagonally without takedown" (isLegalMove initialBoard (1,1) (2,2)) False
  , testCase "Invalid Move Rook - Diagonal" $
      assertEqual "Rook cannot move diagonally" (isLegalMove initialBoardWithoutPawn (0,0) (1,1)) False
  , testCase "Invalid Move Rook - Jump over" $
      assertEqual "Rook cannot jump over other pieces" (isLegalMove initialBoard (0,0) (0,2)) False
  , testCase "Invalid Move Knight - Straight" $
      assertEqual "Knight cannot move straight" (isLegalMove initialBoardWithoutPawn (0,1) (0,2)) False
  , testCase "Invalid Move Knight - Diagonal" $
      assertEqual "Knight cannot move diagonally" (isLegalMove initialBoardWithoutPawn (0,1) (1,2)) False
  , testCase "Invalid Move Bishop - Straight" $
      assertEqual "Bishop cannot move straight" (isLegalMove initialBoardWithoutPawn (0,2) (0,3)) False
  , testCase "Invalid Move Bishop - Jump over" $
      assertEqual "Bishop cannot jump over other pieces" (isLegalMove initialBoard (0,2) (0,4)) False
  , testCase "Invalid Move Queen - Jump over" $
      assertEqual "Queen cannot jump over other pieces" (isLegalMove initialBoard (0,3) (0,5)) False
  , testCase "Invalid Move King - 2 steps" $
      assertEqual "King cannot move 2 steps" (isLegalMove initialBoard (0,4) (0,6)) False
  , testCase "Castling" $
      assertEqual "Castling" (isLegalMove initialCastlingBoard (0,7) (0,4)) True
  , testCase "Invalid Castling" $
      assertEqual "No Castling when path blocked" (isLegalMove initialBoard (0,7) (0,4)) False
  ]

boardTests :: TestTree
boardTests = testGroup "Board"
  [ testCase "Board Row Dimensions" $
      assertEqual "Board should be 8x8" (length initialBoard) 8
    , testCase "Board Column Dimensions" $
      assertEqual "Board should be 8x8" (length (head initialBoard)) 8
    , testCase "Board Cell Color" $
      assertEqual "Cell color should be black" (head (head initialBoard)^. cellColor) black
    , testCase "Board Cell Colors" $
      assertEqual "Cell colors should alternate" (map (^. cellColor) (head initialBoard)) [black, white, black, white, black, white, black, white]
    , testCase "Board Cell Color" $
      assertEqual "Cell color should be white" ((head initialBoard !! 1) ^. cellColor) white
    , testCase "Board Cell Colors" $
      assertEqual "Cell colors should be white" (map (^. cellColor) (initialBoard !! 1)) [white, black, white, black, white, black, white, black]
  ]

pieceTests :: TestTree
pieceTests = testGroup "Pieces"
  [
    testCase "Board Cell Piece" $
      assertEqual "Cell piece should be a pawn" (getPieceType $ head (initialBoard !! 1) ^. cellPiece) (Just Pawn)
    , testCase "Board Cell Pieces" $
      assertEqual "Cell pieces should be pawns" (map (\a -> getPieceType (a ^. cellPiece)) (initialBoard !! 1)) (replicate 8 (Just Pawn))
    , testCase "Board Pieces" $
      assertEqual "Board pieces - first row" (map (\a -> getPieceType (a ^. cellPiece)) (head initialBoard)) [Just Rook, Just Knight, Just Bishop, Just Queen, Just King, Just Bishop, Just Knight, Just Rook]
    , testCase "Board Pieces" $
      assertEqual "Board pieces - last row" (map (\a -> getPieceType (a ^. cellPiece)) (initialBoard !! 7)) [Just Rook, Just Knight, Just Bishop, Just Queen, Just King, Just Bishop, Just Knight, Just Rook]
    , testCase "Board Pieces" $
      assertEqual "Board pieces - middle row" (map (\a -> getPieceType (a ^. cellPiece)) (initialBoard !! 5)) (replicate 8 Nothing)
    , testCase "Board No Piece Colors" $
      assertEqual "Board piece colors - middle row" (map (\a -> getPieceColor' (a ^. cellPiece)) (initialBoard !! 5)) (replicate 8 Nothing)
    , testCase "Board No Piece Colors" $
      assertEqual "Board piece colors - first row" (map (\a -> getPieceColor' (a ^. cellPiece)) (head initialBoard)) (replicate 8 $ Just white)
    , testCase "Board No Piece Colors" $
      assertEqual "Board piece colors - last row" (map (\a -> getPieceColor' (a ^. cellPiece)) (initialBoard !! 7)) (replicate 8 $ Just black)
  ]

checkTest :: TestTree
checkTest = testGroup "Check"
  [
    testCase "Is Check" $
      assertEqual "Is Check" (isKingCheck initialBoard White) False,
    testCase "Is Check" $
      assertEqual "Is Check" (isKingCheck initialCheckBoard Black) True
  ]

checkMateTest :: TestTree
checkMateTest = testGroup "CheckMate"
  [
    testCase "Is CheckMate" $
      assertEqual "Is CheckMate" (isCheckMate initialBoard White) False,
    testCase "Is CheckMate" $
      assertEqual "Is CheckMate" (isCheckMate initialCheckMateBoard Black) True
  ]

getPieceType :: Maybe Piece -> Maybe PieceType
getPieceType Nothing = Nothing
getPieceType (Just p) = Just (p ^. pieceType)

getPieceColor' :: Maybe Piece -> Maybe Color
getPieceColor' Nothing = Nothing
getPieceColor' (Just p) = Just (p ^. pieceColor)


-- genRandomNonCornerCell :: Gen (Int, Int)
-- genRandomNonCornerCell = do
--   x <- choose (1,6)
--   y <- choose (1,6)
--   return (x,y)

-- uniqueColor :: Property
-- uniqueColor = forAll genRandomNonCornerCell $ \(x,y) ->
--   cellColor (initialBoard !! x !! y) /= cellColor (initialBoard !! x-1 !! y)
--     && cellColor (initialBoard !! x !! y) /= cellColor (initialBoard !! x !! y-1)
--     && cellColor (initialBoard !! x !! y) /= cellColor (initialBoard !! x+1 !! y)
--     && cellColor (initialBoard !! x !! y) /= cellColor (initialBoard !! x !! y+1)

-- >>> initialBoard !! 2 !! 0
-- Cell {_cellColor = ISOColor 0, _cellPiece = Nothing, _borderColor = ISOColor 0}
