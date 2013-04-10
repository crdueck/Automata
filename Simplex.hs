import Data.Bits
import qualified Data.Vector.Unboxed as U
import System.Random

grad3 :: U.Vector (Float, Float, Float)
grad3 = U.fromList
    [ (1, 1, 0), (-1, 1, 0), (1, -1, 0), (-1, -1, 0)
    , (1, 0, 1), (-1, 0, 1), (1, 0, -1), (-1, 0, -1)
    , (0, 1, 1), (0, -1, 1), (0, 1, -1), (0, -1, -1) ]

permutation :: StdGen -> U.Vector Int
permutation = U.concat . replicate 2 . U.unsafeBackpermute vec . rvec
    where vec  = U.enumFromN 0 256
          rvec = U.fromListN 256 . randomRs (0,255)

dot3D :: (Float, Float, Float) -> (Float, Float, Float) -> Float
dot3D (a, b, c) (x, y, z) = a * x + b * y + c * z

simplex :: U.Vector Int -> (Float, Float, Float) -> Float
simplex p (x, y, z) = 32 * (n gi0 xyz0 + n gi1 xyz1 + n gi2 xyz2 + n gi3 xyz3)
    where (i, j, k) = (skew x, skew y, skew z)
          skew   a  = ffloor $ a + ((x + y + z) / 3)
          ffloor a  = truncate $ if a > 0 then a else a - 1

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

main :: IO ()
main = do
    let p = U.enumFromN 0 256
    print $ sum [simplex p (x, y, z) | x <- [1..128], y <- [1..256], z <- [1..128]]
