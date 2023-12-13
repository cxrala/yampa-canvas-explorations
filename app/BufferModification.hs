{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import FRP.Yampa.Canvas
import FRP.Yampa
import Data.AffineSpace
import Data.Point2
import Buffer.PixelCanvas

import Graphics.Blank hiding (Event)
import qualified Graphics.Blank as Blank
import qualified Data.Text as Text
import GHC.TypeLits (Div)

--------- setup ---------

main :: IO ()
main = 
    let options = 3000 { events = ["mousedown", "mouseup", "mousemove"] } in
    blankCanvas options draw

draw :: DeviceContext -> IO ()
draw = reactimateSFinContext detectMousedown renderScene (fmap imageData eventsToState)

--------- Data  ---------

-- defining the events the SF 
data MouseEvent = Mousedown (Point2 Double) | Mousemove (Point2 Double) | Mouseup (Point2 Double)

-----------------------------------
-- signal function, SF (Event MouseEvent) ImageData
-- takes a mouse event, and perhaps creates a line (or creates nothing)

data State = State { mouseDown :: Bool, lastMousePos :: Point2 Double, imageData :: ImageData }

eventsToState :: SF (Event MouseEvent) State
eventsToState = sscan updateState (State False undefined (canvasBuilder 100 200))
    where updateState state NoEvent = state
          updateState state (Event e) = case e of
              Mousedown p -> State True p (imageData state)
              Mouseup _ -> State False undefined (imageData state)
              Mousemove p | mouseDown state -> state {
                  lastMousePos = p,
                  imageData = setPixel (ColouredPixel{r=255, g=0, b=0, a=255}) p (imageData state)
              }
              _ -> state

-----------------------------------
-- actuate, ImageData -> Canvas()
-- takes an ImageData and renders the canvas from the ImageData

renderScene :: ImageData -> Canvas ()
renderScene imgdata = putImageData (imgdata, [0, 0])

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
