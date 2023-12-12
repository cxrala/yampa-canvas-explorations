module PixelCanvas where

import Data.Word (Word8)
import qualified Data.Vector.Unboxed as VU
import Graphics.Blank hiding (Event)
import Data.Point2
import qualified Data.Vector.Unboxed.Mutable as MVU
import GHC.Float.RealFracMethods


data PixelCanvas = PixelCanvas {
    height  :: Int,
    width   :: Int
} deriving (Show)

data ColouredPixel = ColouredPixel {
    -- values constrained from 0 to 255
    r       :: Int,
    g       :: Int,
    b       :: Int,
    a       :: Int
} deriving (Show)


-- builds an empty ImageData (ie an empty buffer)
canvasBuilder :: PixelCanvas -> ImageData
canvasBuilder PixelCanvas{height, width} = 
    ImageData height width $ replicate (h * w * 4) (fromIntegral 0)

-- 2d coordinate to row-major rgba index START point (stride is 4)
vecIndexFromPoint :: Point2 Double -> Int -> Int
vecIndexFromPoint (Point2 x y) width =
    let stride = 4 in (floorDoubleInt x) * stride + (floorDoubleInt y) * stride * width

-- gives you a function that will write the next three elements from index i with the coloured pixel values
writeColouredPixel :: ColouredPixel -> Int -> VU.Vector Word8 -> VU.Vector Word8
writeColouredPixel ColouredPixel{r, g, b, a} i vec =
    let modify idx val prev = VU.modify (\v -> MVU.write v idx (fromIntegral val)) prev
    in  modify (i + 3) a $ modify (i + 2) b $ modify (i + 1) g $ modify i r vec
    -- let vr = VU.modify (\v -> MVU.write v i (fromIntegral r)) vec
    -- in let vg = VU.modify (\v -> MVU.write v (i + 1) (fromIntegral g)) vr
    -- in let vb = VU.modify (\v -> MVU.write v (i + 2) (fromIntegral b)) vg
    -- in VU.modify (\v -> MVU.write v (i + 3) (fromIntegral a)) vb

-- sets a value at a specific coordinate to a pixel
setPixel :: ColouredPixel -> Point2 Double -> ImageData -> ImageData
setPixel pixel coord (ImageData w h d) =
    let v = writeColouredPixel pixel (vecIndexFromPoint coord) d in
    ImageData w h v