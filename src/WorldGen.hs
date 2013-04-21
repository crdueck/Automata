module WorldGen (HeightMap, heightMap) where
import Data.Array.Repa
import Simplex

type HeightMap = Array U DIM2 Float

i2f :: Int -> Float
i2f = fromIntegral

heightMap :: DIM2 -> HeightMap
heightMap sh = computeUnboxedS $ noise 3 10 +^ noise 3 20 +^ noise 3 40 +^ noise 5 0.25
    where noise octave freq = fromFunction sh $ \(Z :. x :. y) ->
              harmonic2D octave freq (i2f x) (i2f y)
