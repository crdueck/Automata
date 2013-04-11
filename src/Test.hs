import Data.Array.Repa
import Data.Array.Repa.IO.DevIL
import Data.Word
import Simplex

luminosity :: (DIM3 -> Float) -> DIM3 -> Word8
luminosity _ (Z :. _ :. _ :. 3) = 255
luminosity f (Z :. i :. j :. _) = ceiling $ r + g + b
    where r = f (Z :. i :. j :. 0)
          g = f (Z :. i :. j :. 1)
          b = f (Z :. i :. j :. 2)

noise :: DIM3 -> Array D DIM3 Float
noise sh = fromFunction sh $ \(Z :. x :. y :. z) ->
    simplex3D seed (fromIntegral x, fromIntegral y, fromIntegral z)

main = do
    out <- computeP (traverse (noise (ix3 1080 720 4)) id luminosity)
    runIL $ writeImage "out.png" (RGB out)
