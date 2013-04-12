module Simplex
    ( harmonic2D
    , simplex2D
    , simplex2D'
    , harmonic3D
    , simplex3D
    , simplex3D'
    ) where
import Data.Bits
import qualified Data.Vector.Unboxed as U

harmonic2D :: Int -> Float -> Float -> Float -> Float
harmonic2D octave freq x y = harmonic noise octave
    where noise o = simplex2D (x * o / freq) (y * o / freq)

harmonic3D :: Int -> Float -> Float -> Float -> Float -> Float
harmonic3D octave freq x y z = harmonic noise octave
    where noise o = simplex3D (x * o / freq) (y * o / freq) (z * o / freq)

harmonic :: (Float -> Float) -> Int -> Float
harmonic noise octave = go octave / (2 - 1 / (2 ^ (octave - 1)))
    where go 0 = 0
          go o = let r = i2f (1 `shiftL` (o - 1)) in noise r / r + go (o - 1)

simplex2D :: Float -> Float -> Float
{-# INLINE simplex2D #-}
simplex2D = simplex2D' seed

simplex2D' :: U.Vector Int -> Float -> Float -> Float
simplex2D' p x y = 70 * (n gi0 xy0 + n gi1 xy1 + n gi2 xy2)
    where (i, j) = (skew x, skew y)
          skew a = floor $ a + ((x + y) * 0.5 * (sqrt 3 - 1)) :: Int

          g = (3 - sqrt 3) / 6
          t = i2f (i + j) * g
          xy0@(x0, y0) = (x - i2f i + t, y - i2f j + t)

          (i0, j0) = if x0 > y0 then (1, 0) else (0, 1)

          xy1 = (x0 - i2f i0 +     g, y0 - i2f j0 +     g)
          xy2 = (x0 - 1      + 2 * g, y0 - 1      + 2 * g)

          (ii, jj) = (i .&. 255, j .&. 255)

          gi0 = (p `U.unsafeIndex` ii      + p `U.unsafeIndex` jj     ) `rem` 12
          gi1 = (p `U.unsafeIndex` ii + i0 + p `U.unsafeIndex` jj + j0) `rem` 12
          gi2 = (p `U.unsafeIndex` ii + 1  + p `U.unsafeIndex` jj + 1 ) `rem` 12

          n gi (a, b) =
            let s = 0.5 - a*a - b*b
            in if s < 0 then 0 else (s*s*s*s) * dot3D (grad3D `U.unsafeIndex` gi) (a, b, 0)

simplex3D :: Float -> Float -> Float -> Float
{-# INLINE simplex3D #-}
simplex3D = simplex3D' seed

simplex3D' :: U.Vector Int -> Float -> Float -> Float -> Float
simplex3D' p x y z = 32 * (n gi0 xyz0 + n gi1 xyz1 + n gi2 xyz2 + n gi3 xyz3)
    where (i, j, k) = (skew x, skew y, skew z)
          skew a = floor $ a + ((x + y + z) / 3) :: Int

          t = i2f (i + j + k) / 6
          xyz0@(x0, y0, z0) = (x - i2f i + t, y - i2f j + t, z - i2f k + t)

          (i0, j0, k0, i1, j1, k1) = if x0 >= y0 then ijk1 else ijk2

          ijk1 | y0 >= z0  = (1, 0, 0, 1, 1, 0)
               | x0 >= z0  = (1, 0, 0, 1, 0, 1)
               | otherwise = (0, 0, 1, 1, 0, 1)

          ijk2 | y0 < z0   = (0, 0, 1, 0, 1, 1)
               | x0 < z0   = (0, 1, 0, 0, 1, 1)
               | otherwise = (0, 1, 0, 1, 1, 0)

          xyz1 = (x0 - i2f i0 + 1/6, y0 - i2f j0 + 1/6, z0 - i2f k0 + 1/6)
          xyz2 = (x0 - i2f i1 + 1/3, y0 - i2f j1 + 1/3, z0 - i2f k1 + 1/3)
          xyz3 = (x0 - 1      + 1/2, y0 - 1      + 1/2, z0 - 1      + 1/2)

          (ii, jj, kk) = (i .&. 255, j .&. 255, k .&. 255)

          gi0 = (p `U.unsafeIndex` ii      + p `U.unsafeIndex` jj      + p `U.unsafeIndex` kk     ) `rem` 12
          gi1 = (p `U.unsafeIndex` ii + i0 + p `U.unsafeIndex` jj + j0 + p `U.unsafeIndex` kk + k0) `rem` 12
          gi2 = (p `U.unsafeIndex` ii + i1 + p `U.unsafeIndex` jj + j1 + p `U.unsafeIndex` kk + k1) `rem` 12
          gi3 = (p `U.unsafeIndex` ii + 1  + p `U.unsafeIndex` jj + 1  + p `U.unsafeIndex` kk + 1 ) `rem` 12

          n gi xyz@(a, b, c) =
              let s = 0.5 - a*a - b*b - c*c
              in if s < 0 then 0 else (s*s*s*s) * dot3D (grad3D `U.unsafeIndex` gi) xyz

i2f :: Int -> Float
{-# INLINE i2f #-}
i2f = fromIntegral

dot3D :: (Float, Float, Float) -> (Float, Float, Float) -> Float
{-# INLINE dot3D #-}
dot3D (a, b, c) (x, y, z) = a * x + b * y + c * z

grad3D :: U.Vector (Float, Float, Float)
grad3D = U.fromList
    [ (1, 1, 0), (-1, 1, 0), (1, -1, 0), (-1, -1, 0)
    , (1, 0, 1), (-1, 0, 1), (1, 0, -1), (-1, 0, -1)
    , (0, 1, 1), (0, -1, 1), (0, 1, -1), (0, -1, -1) ]

seed :: U.Vector Int
seed = U.fromList [54,49,73,185,1,155,81,144,6,31,143,175,14,252,164,200,130,221,95,191,157,178,43,188,254,134,151,63,216,62,65,48,108,166,72,60,181,239,61,223,20,165,118,113,3,234,220,251,59,24,19,26,87,12,30,7,38,219,179,201,52,211,75,237,162,149,68,218,25,89,195,106,163,128,77,208,100,79,242,136,186,35,160,231,44,121,2,229,168,182,167,184,27,215,173,110,213,88,124,123,115,76,255,253,74,199,148,107,28,69,146,15,241,245,32,64,238,205,125,80,202,210,84,37,9,207,176,56,171,111,16,102,22,78,98,91,135,97,126,66,140,172,198,46,86,40,101,131,183,83,150,41,51,145,243,236,240,132,230,36,224,119,222,39,141,18,127,170,94,0,233,248,117,247,45,197,206,250,58,226,42,232,17,21,11,244,156,196,212,53,190,71,180,147,109,103,13,122,192,138,142,34,33,227,137,169,10,105,55,114,153,93,92,203,57,120,217,47,204,29,85,129,174,159,158,249,90,209,104,112,225,187,4,96,161,67,8,133,50,194,82,235,23,99,193,154,189,177,5,152,246,116,214,139,70,228, 54,49,73,185,1,155,81,144,6,31,143,175,14,252,164,200,130,221,95,191,157,178,43,188,254,134,151,63,216,62,65,48,108,166,72,60,181,239,61,223,20,165,118,113,3,234,220,251,59,24,19,26,87,12,30,7,38,219,179,201,52,211,75,237,162,149,68,218,25,89,195,106,163,128,77,208,100,79,242,136,186,35,160,231,44,121,2,229,168,182,167,184,27,215,173,110,213,88,124,123,115,76,255,253,74,199,148,107,28,69,146,15,241,245,32,64,238,205,125,80,202,210,84,37,9,207,176,56,171,111,16,102,22,78,98,91,135,97,126,66,140,172,198,46,86,40,101,131,183,83,150,41,51,145,243,236,240,132,230,36,224,119,222,39,141,18,127,170,94,0,233,248,117,247,45,197,206,250,58,226,42,232,17,21,11,244,156,196,212,53,190,71,180,147,109,103,13,122,192,138,142,34,33,227,137,169,10,105,55,114,153,93,92,203,57,120,217,47,204,29,85,129,174,159,158,249,90,209,104,112,225,187,4,96,161,67,8,133,50,194,82,235,23,99,193,154,189,177,5,152,246,116,214,139,70,228]
