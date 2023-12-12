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
import GHC.TypeLits (Div)

--------- setup ---------

main :: IO ()
main = 
    let options = 3000 { events = ["mousedown", "mouseup", "mousemove"] } in
    blankCanvas options draw

draw :: DeviceContext -> IO ()
draw = reactimateSFinContext detectMousedown renderScene (fmap linesDrawn eventsToState)

--------- Data  ---------

-- defining the events the SF 
data MouseEvent = Mousedown (Point2 Double) | Mousemove (Point2 Double) | Mouseup (Point2 Double)
newtype Line = Line (Point2 Double, Point2 Double)


-----------------------------------
-- signal function, SF (Event MouseEvent) (Maybe Line)
-- takes a mouse event, and perhaps creates a line (or creates nothing)

data State = State { mouseDown :: Bool, lastMousePos :: Point2 Double, linesDrawn :: [Line] }

eventsToState :: SF (Event MouseEvent) State
eventsToState = sscan updateState (State False undefined [])
    where updateState state NoEvent = state
          updateState state (Event e) = case e of
              Mousedown p -> State True p (linesDrawn state)
              Mouseup _ -> State False undefined (linesDrawn state)
              Mousemove p | mouseDown state -> state {
                  lastMousePos = p,
                  linesDrawn = Line (lastMousePos state, p) : linesDrawn state
              }
              _ -> state

-----------------------------------
-- actuate, Maybe Line -> Canvas()
-- takes a line and renders the canvas from the line

renderScene :: [Line] -> Canvas ()
renderScene = mapM_ renderLine

renderLine :: Line -> Canvas ()
renderLine (Line (Point2 x1 y1, Point2 x2 y2)) =
    do
        beginPath ()
        moveTo (x1, y1)
        lineTo (x2, y2)
        lineWidth 1
        stroke ()

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
