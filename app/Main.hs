{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank
import FRP.Yampa.Canvas
import FRP.Yampa
import Data.AffineSpace

-- main = blankCanvas 3000 $ \context -> do -- start blank canvas on port 3000
--         send context $ do                 -- send commands to this specific context
--                 beginPath()
--                 moveTo(50,50)
--                 lineTo(200,100)
--                 lineWidth 3
--                 stroke()                  -- this draws the ink into the canvas

main :: IO ()
main = blankCanvas 3000 { events = ["mousedown", "mouseup"] } createLine

createLine  :: DeviceContext -> IO ()
createLine = reactimateSFinContext detectMouseDown 


------------------------------------------------

-- rendering!

renderScene :: [PixelPoint] -> Canvas ()
renderScene points = 

renderPoint :: PixelPoint -> Canvas ()
renderPoint p =
        let Point2 x y = pos p
        

scaleScene :: Canvas ()
scaleScene =
   do context <- myCanvasContext
      let w = width context
          h = height context
      translate (0,h)
      scale (w, negate h)

------------------------------------------------

-- definitions!

-- a very cursed definition of point. change to bitmap interpretation later.
data PixelPoint = MkPixelPoint {
        pos     :: Point2,
        size    :: Integer
}

pointPixel :: Point2 -> PixelPoint
pointPixel p = MkPixelPoint {
        pos     = p,
        size    = 1
}

------------------------------------------------


-- probably specify here that it's actually a mousedown event (case eType)
detectMouseDown :: Blank.Event -> Canvas (Event Point2)
detectMouseDown ev = case ePageXY ev of
                        Nothing -> return NoEvent
                        Just (x, y) -> fmap Event (toXYCo (x, y))

toXYCo :: (Double,Double) -> Canvas Position
toXYCo (i,j) =
  do context <- myCanvasContext
     let w = width context
         h = height context
     return $ Point2 (i / w) (1 - j / h)