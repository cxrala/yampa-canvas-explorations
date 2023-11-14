{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import FRP.Yampa.Canvas
import FRP.Yampa
import Data.AffineSpace
import Data.Point2

import Graphics.Blank hiding (Event)
import qualified Graphics.Blank as Blank
import Data.Text as Text
import qualified Data.Text as Text

-- main :: IO ()
-- main = blankCanvas 3000 $ \context -> do -- start blank canvas on port 3000
--         send context $ do                 -- send commands to this specific context
--                 beginPath()
--                 moveTo(50,50)
--                 lineTo(200,100)
--                 lineWidth 3
--                 stroke()                  -- this draws the ink into the canvas

main :: IO ()
main = blankCanvas 3000 { events = ["mousedown", "mouseup", "mousemove"] } draw

data MouseEvent = Mousedown (Point2 Double) | Mousemove (Point2 Double) | Mouseup (Point2 Double)
newtype Line = (Point2 Double, Point2 Double) 

draw :: DeviceContext -> IO ()
draw = reactimateSFinContext detectMousedown

-----------------------------------
-- signal function, SF (Event MouseEvent) (Maybe Line)
-- takes a mouse event, and perhaps creates a line (or creates nothing)

-----------------------------------
-- actuate, (Maybe Line) -> Canvas()
-- takes a line and renders the canvas from the line

renderScene :: 


-----------------------------------
-- sense, Blank.Event -> Canvas (Event MouseEvent)

detectMousedown :: Blank.Event -> Canvas (Event MouseEvent)
detectMousedown ev =
    case ePageXY ev of
        Nothing -> return NoEvent
        Just (x, y) -> return $ Event $
                            case eType ev of
                                "mousedown" -> Mousedown $ Point2 x y
                                "mouseup" -> Mouseup $ Point2 x y
                                "mousemove" -> Mousemove $ Point2 x y
