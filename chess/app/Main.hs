{-# LANGUAGE TemplateHaskell #-}

module Main where

-- import Brick

-- ui :: Widget ()
-- ui = str "Hello, world!"

-- main :: IO ()
-- main = simpleMain ui

-- draw a chess board

import Brick
import Brick.Main
import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.Border.Style
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Data.Maybe
import Control.Monad
import Types

cellAttr :: Cell -> AttrName
cellAttr c = attrName $ fromMaybe def $ c^.cellPiece._Just.pieceColor

cellWidget :: Cell -> Widget ()
cellWidget c = withAttr (cellAttr c) $ str "  "

boardWidget :: Board -> Widget ()
boardWidget b = vBox $ map hBox $ map (map cellWidget) $ b

drawUI :: Board -> [Widget ()]
drawUI b = [center $ padAll 1 $ withBorderStyle unicode $ borderWithLabel (str "Chess") $ boardWidget b]

-- appEvent :: Board -> BrickEvent n e -> EventM n (Next Board)
-- appEvent b (VtyEvent (EvKey (KChar 'q') [])) = halt b
-- appEvent b (VtyEvent (EvKey (KChar 'r') [])) = continue initialState
-- appEvent b _ = continue b

initialState :: Board
initialState = replicate 8 $ replicate 4 $ Cell white Nothing -- `concat` (Cell black Nothing)

-- place white cell beside black cell
-- boardAttr :: Board -> AttrMap
-- boardAttr b = attrMap def $ map (\c -> (cellAttr c, def `withForeColor` (c^.cellColor))) $ concat $ b^.boardCells

-- app :: App Board e ()
-- app = App { appDraw = drawUI
--           , appChooseCursor = neverShowCursor
--           , appHandleEvent = appEvent
--           , appStartEvent = return
--           , appAttrMap = const $ boardAttr initialState
--           }


main :: IO ()
main = do
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just $ B.boardAttr initialState) app initialState