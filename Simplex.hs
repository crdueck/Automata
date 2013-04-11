{-# LANGUAGE BangPatterns #-}

import Data.Array.Repa as R
import Data.Array.Repa.Repr.Cursored
import Data.Bits
import Data.List (foldl1')
import Data.Word
import qualified Data.Vector.Unboxed as U

{-import System.Random-}
{-permutation :: StdGen -> U.Vector Int-}
{-permutation = U.concat . replicate 2 . U.unsafeBackpermute vec . rvec-}
    {-where vec  = U.enumFromN 0 256-}
          {-rvec = U.fromListN 256 . randomRs (0,255)-}

seed :: U.Vector Int
seed = U.fromList [15,215,65,220,5,141,7,142,6,6,122,94,253,124,123,183,142,137,36,136,160,15,180,221,1,251,124,179,135,191,35,60,156,253,15,138,153,43,202,131,116,22,115,115,76,58,224,192,77,71,96,158,159,4,89,183,196,161,33,59,40,120,53,172,191,174,239,110,31,193,70,148,237,109,245,193,35,255,78,187,80,80,29,104,173,254,5,101,104,57,49,2,125,17,35,1,84,54,51,138,73,41,109,58,183,58,204,137,241,221,138,251,8,228,92,190,98,231,166,10,180,14,63,204,167,206,234,114,210,55,70,25,148,112,120,144,52,216,77,225,239,101,125,41,69,192,121,249,15,28,131,242,62,8,51,254,158,56,10,40,27,233,186,227,226,210,9,19,26,168,113,165,221,188,252,48,51,58,148,194,116,200,246,215,93,82,54,7,53,134,74,131,206,139,141,228,168,26,232,176,55,109,67,207,44,96,94,169,229,190,213,117,178,170,196,238,86,148,174,156,186,43,60,155,5,109,235,219,187,93,102,25,18,129,104,150,234,53,18,160,5,24,39,111,126,48,120,152,39,45,224,183,105,184,22,243,15,215,65,220,5,141,7,142,6,6,122,94,253,124,123,183,142,137,36,136,160,15,180,221,1,251,124,179,135,191,35,60,156,253,15,138,153,43,202,131,116,22,115,115,76,58,224,192,77,71,96,158,159,4,89,183,196,161,33,59,40,120,53,172,191,174,239,110,31,193,70,148,237,109,245,193,35,255,78,187,80,80,29,104,173,254,5,101,104,57,49,2,125,17,35,1,84,54,51,138,73,41,109,58,183,58,204,137,241,221,138,251,8,228,92,190,98,231,166,10,180,14,63,204,167,206,234,114,210,55,70,25,148,112,120,144,52,216,77,225,239,101,125,41,69,192,121,249,15,28,131,242,62,8,51,254,158,56,10,40,27,233,186,227,226,210,9,19,26,168,113,165,221,188,252,48,51,58,148,194,116,200,246,215,93,82,54,7,53,134,74,131,206,139,141,228,168,26,232,176,55,109,67,207,44,96,94,169,229,190,213,117,178,170,196,238,86,148,174,156,186,43,60,155,5,109,235,219,187,93,102,25,18,129,104,150,234,53,18,160,5,24,39,111,126,48,120,152,39,45,224,183,105,184,22,243]

grad3 :: U.Vector (Float, Float, Float)
grad3 = U.fromList
    [ (1, 1, 0), (-1, 1, 0), (1, -1, 0), (-1, -1, 0)
    , (1, 0, 1), (-1, 0, 1), (1, 0, -1), (-1, 0, -1)
    , (0, 1, 1), (0, -1, 1), (0, 1, -1), (0, -1, -1) ]

dot3D :: (Float, Float, Float) -> (Float, Float, Float) -> Float
dot3D (a, b, c) (x, y, z) = a * x + b * y + c * z

simplex :: U.Vector Int -> (Float, Float, Float) -> Float
simplex p (x, y, z) = 32 * (n gi0 xyz0 + n gi1 xyz1 + n gi2 xyz2 + n gi3 xyz3)
    where (i, j, k) = (skew x, skew y, skew z)
          skew a = ffloor $ a + ((x + y + z) / 3)

          ffloor :: Float -> Int
          ffloor = floor

          t = fromIntegral (i + j + k) / 6
          xyz0@(x0, y0, z0) = (x - i' + t, y - j' + t, z - k' + t)

          (i0, j0, k0, i1, j1, k1) = if x0 >= y0 then ijk1 else ijk2

          ijk1
              | y0 >= z0  = (1, 0, 0, 1, 1, 0)
              | x0 >= z0  = (1, 0, 0, 1, 0, 1)
              | otherwise = (0, 0, 1, 1, 0, 1)
          ijk2
              | y0 < z0   = (0, 0, 1, 0, 1, 1)
              | x0 < z0   = (0, 1, 0, 0, 1, 1)
              | otherwise = (0, 1, 0, 1, 1, 0)

          (i',  j',  k')  = (fromIntegral i,  fromIntegral j,  fromIntegral k)
          (i0', j0', k0') = (fromIntegral i0, fromIntegral j0, fromIntegral k0)
          (i1', j1', k1') = (fromIntegral i1, fromIntegral j1, fromIntegral k1)

          xyz1 = (x0 - i0' + (1/6), y0 - j0' + (1/6), z0 - k0' + (1/6))
          xyz2 = (x0 - i1' + (1/3), y0 - j1' + (1/3), z0 - k1' + (1/3))
          xyz3 = (x0 - 1   + (1/2), y0 - 1   + (1/2), z0 - 1   + (1/2))

          ii = i .&. 255
          jj = j .&. 255
          kk = k .&. 255

          (!) = U.unsafeIndex

          gi0 = (p ! ii      + (p ! jj      + (p ! kk     ))) `rem` 12
          gi1 = (p ! ii + i0 + (p ! jj + j0 + (p ! kk + k0))) `rem` 12
          gi2 = (p ! ii + i1 + (p ! jj + j1 + (p ! kk + k1))) `rem` 12
          gi3 = (p ! ii + 1  + (p ! jj + 1  + (p ! kk + 1 ))) `rem` 12

          n gi xyz'@(x', y', z') =
              let s = 0.6 - x'*x' - y'*y' - z'*z'
              in if s < 0 then 0 else (s*s*s*s) * dot3D (grad3 `U.unsafeIndex` gi) xyz'

type Region  = Array D DIM3
type World a = Array C DIM3 (Region a)

worldGen :: DIM3 -> World Float
worldGen sh@(Z :. x :. y :. z) = makeCursored (ix3 8 8 8) id addDim getRegion
    where getRegion (Z :. i :. j :. k) =
              let ix = ix3 (i * 8) (j * 8) (k * 8)
                  sz = ix3 (x `quot` 8) (y `quot` 8) (z `quot` 8)
              in extract ix sz world
          world = fromFunction sh $ \(Z :. x :. y :. z) ->
                simplex seed (fromIntegral x, fromIntegral y, fromIntegral z)

subGrid sh = fromFunction sh $ \(Z :. x :. y) ->
    simplex seed (fromIntegral x, fromIntegral y, 0)

text :: Array D DIM2 Float -> IO ()
text grid = mapM_ (print . toList . R.map ascii . row) [0..nRows - 1]
    where row :: Int -> Array D R.DIM1 Float
          row i = slice grid $ R.Any :. i :. R.All
          ascii n | n >= 0 = '.'
          ascii _ = '#'
          nRows = case extent grid of Z :. x :. _ -> x

{-main = text $ subGrid (ix2 1000 1000)-}
main = text $ subGrid (ix2 50 100)

{-main :: IO ()-}

{-main = do-}
    {-print $ foldl1' (+) [simplex seed (x, y, z) | x <- [1..128], y <- [1..128], z <- [1..128]]-}
    {-print [simplex seed (x, y, z) | x <- [1..10], y <- [1..10], z <- [1..10]]-}
