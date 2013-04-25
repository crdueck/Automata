{-# LANGUAGE QuasiQuotes #-}
module WorldGen (Region, regionGen, World, worldGen) where
import Data.Array.Repa as R
import Data.Array.Repa.Repr.Cursored
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Simplex

type Region = Array U DIM2 Float
type World  = Array C DIM2 Region

sten1 :: Stencil DIM2 Float
sten1 = [stencil2| 1 1 1
                   1 0 1
                   1 1 1 |]

regionGen :: DIM2 -> Region
regionGen sh = computeUnboxedS $ noise 5 25 +^ noise 3 45 +^ noise 3 110 +^ noise 5 220
    where noise octave freq = fromFunction sh $ \(Z :. x :. y) ->
              harmonic2D octave freq (fromIntegral x) (fromIntegral y)

worldGen :: DIM2 -> World
worldGen sh = makeCursored sh id addDim getRegion
    where getRegion _ = regionGen (ix2 128 128)

    {-where getRegion start@(Z :. x :. y) =-}
              {-let end = Z :. x + 128 :. y + 128-}
              {-in computeUnboxedS $ extract start end regions-}
          {-regions = fromFunction sh $ \(Z :. i :. j) ->-}
              {-simplex2D (fromIntegral i) (fromIntegral j)-}
