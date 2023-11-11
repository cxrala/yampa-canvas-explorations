{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import FRP.Yampa.Canvas
import FRP.Yampa
import Data.AffineSpace
import Data.Point2
import Graphics.Blank hiding (Event)
import qualified Graphics.Blank as Blank

-- main = blankCanvas 3000 $ \context -> do -- start blank canvas on port 3000
--         send context $ do                 -- send commands to this specific context
--                 beginPath()
--                 moveTo(50,50)
--                 lineTo(200,100)
--                 lineWidth 3
--                 stroke()                  -- this draws the ink into the canvas

newtype MouseEvent = Mousedown (Point2 Double)

------------------------------------------------

main :: IO ()
main = blankCanvas 3000 { events = ["mousedown", "mouseup"] } createLine

createLine  :: DeviceContext -> IO ()
createLine = reactimateSFinContext detectMousedown renderScene createPixel

------------------------------------------------

-- construct a pixel model that responds to input events by adding new pixels
createPixel :: SF (Event MouseEvent) [PixelPoint]
createPixel = pixelGenerator >>> pixelCollection []

-- a collection of already drawn pixels into which new pixels can be drawn
pixelCollection :: [SF (Event PixelPoint) PixelPoint] -> SF (Event PixelPoint) [PixelPoint]
pixelCollection pixels = notYet >>> pSwitchB pixels (arr fst) (\sfs p -> pixelCollection (stationaryBall p : sfs))

stationaryBall :: forall x. PixelPoint -> SF x PixelPoint
stationaryBall p = arr (const p)

-- convert events carrying co-ordinates into events carrying new pixels
pixelGenerator :: SF (Event MouseEvent) (Event PixelPoint)
pixelGenerator = arr (fmap newPixelAt)

-- create a new pixel at the specific point
newPixelAt :: MouseEvent -> PixelPoint
newPixelAt (Mousedown p) = MkPixelPoint {
        pos     = p,
        size    = 1
}

------------------------------------------------

-- rendering!


renderScene :: [PixelPoint] -> Canvas ()
renderScene points = renderInstructions >> renderPoints points

-- | A Canvas action to render the instruction text.
renderInstructions :: Canvas ()
renderInstructions =
 do font "20pt Comic Sans MS"
    fillText ("Click on the canvas to draw something", 50, 50)

renderPoint :: PixelPoint -> Canvas ()
renderPoint p =
    do
    let Point2 x y = pos p
    let dim = size p
    fillStyle "blue"
    fillRect (x, y, dim, dim)

renderPoints :: [PixelPoint] -> Canvas ()
renderPoints = mapM_ renderPoint

------------------------------------------------

-- definitions!

-- a very cursed definition of point. change to bitmap interpretation later.
data PixelPoint = MkPixelPoint {
        pos     :: Point2 Double,
        size    :: Double
}

------------------------------------------------


-- probably specify here that it's actually a mousedown event (case eType)
detectMousedown :: Blank.Event -> Canvas (Event MouseEvent)
detectMousedown ev = if eType ev == "mouseup" then return NoEvent else
    case ePageXY ev of
        Nothing -> return NoEvent
        Just (x, y) -> return (Event $ Mousedown $ Point2 x y)

