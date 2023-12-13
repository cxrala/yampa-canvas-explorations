{-# LANGUAGE RankNTypes #-}

module Buffer.PixelCanvas (
ColouredPixel(..),
canvasBuilder,
setPixel
) where

import Data.Word (Word8)
import Data.Point2
import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Mutable (MVector)
import Data.Vector.Unboxed (Vector)
import GHC.Float.RealFracMethods
import Control.Monad.ST
import Control.Monad.Primitive
import Graphics.Blank hiding (Event)

data ColouredPixel = ColouredPixel {
    -- values constrained from 0 to 255
    r       :: Word8,
    g       :: Word8,
    b       :: Word8,
    a       :: Word8
} deriving (Show)

-- builds an empty ImageData (ie an empty buffer)
canvasBuilder :: Int -> Int -> ImageData
canvasBuilder h w = 
    ImageData h w $ VU.replicate (h * w * 4) (fromIntegral 0)

-- 2d coordinate to row-major rgba index START point (stride is 4)
vecIndexFromPoint :: Point2 Double -> Int -> Int
vecIndexFromPoint (Point2 x y) width =
    let stride = 4 in (floorDoubleInt x) * stride + (floorDoubleInt y) * stride * width

-- runs op on a vector, giving you a new vector. this is safe (but O(n))
inPlace :: VU.Unbox a => (forall s. MVector (PrimState (ST s)) a -> ST s ()) -> Vector a -> Vector a
inPlace op vec = runST $ do
    mv <- VU.thaw vec
    op mv
    VU.freeze mv

-- writes a coloured pixel into a mutable vector at index i
writeColouredPixel :: forall s. ColouredPixel -> Int -> MVector (PrimState (ST s)) Word8 -> ST s ()
writeColouredPixel pixel i mvec = do
    MVU.write mvec i $ r pixel
    MVU.write mvec (i + 1) $ g pixel
    MVU.write mvec (i + 2) $ b pixel
    MVU.write mvec (i + 3) $ a pixel

-- sets a value at a specific coordinate to a pixel
setPixel :: ColouredPixel -> Point2 Double -> ImageData -> ImageData
setPixel pixel coord (ImageData w h d) =
    let v = inPlace (writeColouredPixel pixel $ vecIndexFromPoint coord w) d in
    ImageData w h v