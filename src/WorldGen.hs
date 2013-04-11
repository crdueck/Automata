import Data.Array.Repa as R
import qualified Data.Array.Repa as R
import Data.Array.Repa.Repr.Cursored
import Simplex

main :: IO ()
main = print $ sumAllS (worldGen (ix3 128 128 128))

type Region  = Array U DIM3
type World a = Array C DIM3 (Region a)

worldGen :: DIM3 -> Array D DIM3 Float
worldGen sh = f 10 0.25 +^ f 20 0.33 +^ f 5 0.75
    where f octave freq = fromFunction sh $ \(Z :. x :. y :. z) ->
            harmonic3D octave freq (fromIntegral x) (fromIntegral y) (fromIntegral z)

text :: Array D DIM2 Float -> IO ()
text grid = mapM_ (print . toList . R.map ascii . row) [0..nRows - 1]
    where row :: Int -> Array D DIM1 Float
          row i = slice grid $ Any :. i :. All
          ascii n | n >= 0 = '.'
          ascii _ = '#'
          nRows = case extent grid of Z :. x :. _ -> x
