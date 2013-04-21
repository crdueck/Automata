module WorldGen (Region, regionGen, World, worldGen) where
import Data.Array.Repa
import Data.Array.Repa.Repr.Cursored
import Simplex

type Region = Array U DIM2 Float
type World  = Array C DIM2 Region

regionGen :: DIM2 -> Region
regionGen sh = computeUnboxedS $ noise 3 10 +^ noise 3 20 +^ noise 3 40 +^ noise 5 0.25
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
