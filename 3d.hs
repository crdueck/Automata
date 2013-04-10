{-# LANGUAGE FlexibleContexts, TypeOperators #-}

import Data.Array.Repa (Array, DIM2, DIM3, D, U, Z(..), (:.)(..), Any(..), All(..))
import qualified Data.Array.Repa as R
import Data.Array.Repa.Repr.Cursored (C, makeCursored)
import Data.Array.Repa.Stencil (Stencil, Boundary(..), makeStencil)
import Data.Array.Repa.Stencil.Dim2 (mapStencil2)
import Data.Word (Word8)
import System.Random

import Debug.Trace

type Region  = Array U DIM3
type World a = Array C DIM3 (Region a)

sten2D :: Stencil DIM2 Word8
sten2D = makeStencil (R.ix2 3 3) $ \sh ->
    case sh of
        Z :. 1 :. 1 -> Nothing
        _ -> Just 1

sten3D :: Stencil DIM3 Word8
sten3D = makeStencil (R.ix3 3 3 3) $ \sh ->
    case sh of
        Z :. 1 :. 1 :. 1 -> Nothing
        _ -> Just 1

stack :: (R.Shape sh, R.Source r c, R.Source r2 c)
      => Array r (sh :. Int) c -> Array r2 sh c -> Array D (sh :. Int) c
stack arr1 arr2 = traceShow "stack" $ R.traverse2 arr1 arr2 resize $ \f g sh@(sh' :. _) ->
    if R.extent arr1 `R.inShape` sh then f sh else g sh'
    where resize :: R.Shape (sh :. Int) => (sh :. Int) -> t -> (sh :. Int)
          resize (xs :. x) _ = xs :. (x + 1)

{-worldGen :: DIM3 -> StdGen -> World Word8-}
worldGen (Z :. x :. y :. z) g = traceShow "worldGen" $ world
    {-makeCursored (R.ix3 8 8 8) id R.addDim getRegion-}
    where getRegion :: DIM3 -> Region Word8
          {-# INLINE getRegion #-}
          getRegion (Z :. i :. j :. k) =
            let ix = R.ix3 (i * 8) (j * 8) (k * 8)
                sz = R.ix3 (x `quot` 8) (y `quot` 8) (z `quot` 8)
            in R.computeUnboxedS $ R.extract ix sz world

          world :: Region Word8
          {-# INLINE world #-}
          world = traceShow "world" $ foldr buildStep z0 [0..z-1]
          {-world = traceShow "world" $ foldr buildStep z0 [0,1]-}

          buildStep :: Int -> Region Word8 -> Region Word8
          {-# INLINE buildStep #-}
          buildStep h z = traceShow "buildStep" $ R.computeUnboxedS $ z `stack` steps (R.slice z (Any :. h :. All))

          steps :: Array D DIM2 Word8 -> Array U DIM2 Word8
          {-# INLINE steps #-}
          steps = iterStep . iterStep . iterStep . R.computeUnboxedS

          -- ground level, pseudo-random noise
          z0 :: Region Word8
          {-# INLINE z0 #-}
          z0 = traceShow "z0" $ R.fromListUnboxed (R.ix3 x y 1) . take (x * y) $ randomRs (0,1) g

step :: (Word8 -> Word8 -> Word8) -> Stencil DIM2 Word8 -> Array U DIM2 Word8 -> Array U DIM2 Word8
step transit sten region = R.computeUnboxedS $ R.zipWith transit region appSten
    where appSten = mapStencil2 (BoundConst 1) sten region

iterStep :: Array U DIM2 Word8 -> Array U DIM2 Word8
iterStep = step transit sten2D
    where transit 0 n = if n > 4 then 1 else 0
          transit _ n = if n > 3 then 1 else 0

_3d :: Region Int
_3d = R.fromListUnboxed (R.ix3 2 2 2) [1..8]

_2d :: Array U DIM2 Int
_2d = R.fromListUnboxed (R.ix2 2 2) [10..13]

{-main = print $ R.toList $ stack _3d _2d-}
main = newStdGen >>= print . R.toList . worldGen (R.ix3 8 8 8)
