{-# LANGUAGE QuasiQuotes #-}

import Data.Array.Repa (Array, DIM2, U, Z(..), (:.)(..))
import qualified Data.Array.Repa as R
import Data.Array.Repa.Repr.Cursored (C, makeCursored)
import Data.Array.Repa.Stencil (Stencil, Boundary(..))
import Data.Array.Repa.Stencil.Dim2 (stencil2, makeStencil2, mapStencil2)
import Data.Word (Word8)
import System.Random

import Bindings
import Data.IORef
import Graphics.UI.GLUT

type SubGrid = Array U DIM2 Word8
type Grid = Array C DIM2 SubGrid

worldGen :: DIM2 -> StdGen -> Grid
worldGen sh@(Z :. x :. y) g = makeCursored (R.ix2 8 8) id R.addDim (R.computeUnboxedS . sub)
    where sub (Z :. i :. j) = R.extract (R.ix2 (i * 8) (j * 8)) (R.ix2 xSz ySz) delayed
          delayed = R.delay $ subGen sh g
          xSz = x `quot` 8
          ySz = y `quot` 8

subGen :: DIM2 -> StdGen -> SubGrid
subGen sh@(Z :. x :. y) = steps . world
    where steps = iterStep . iterStep . iterStep
          world = R.fromListUnboxed sh . take (x * y) . randomRs (0,1)

step :: (Word8 -> Word8 -> Word8) -> Stencil DIM2 Word8 -> SubGrid -> SubGrid
step transit sten grid = R.computeUnboxedS $ R.zipWith transit grid appSten
    where appSten = mapStencil2 (BoundConst 1) sten grid

iterStep :: SubGrid -> SubGrid
iterStep = step transit [stencil2| 1 1 1 1 0 1 1 1 1 |]
    where transit 0 n = if n > 4 then 1 else 0
          transit _ n = if n > 3 then 1 else 0

text :: SubGrid -> IO ()
text grid = mapM_ (print . R.toList . R.map ascii . row) [0..nRows - 1]
    where row :: Int -> Array R.D R.DIM1 Word8
          row i = R.slice grid $ R.Any :. i :. R.All
          ascii 0 = '.'
          ascii _ = '#'
          nRows = case R.extent grid of Z :. x :. _ -> x

main :: IO ()

main = text . subGen (R.ix2 50 100) $ mkStdGen 10

{-main = newStdGen >>= mapM_ text . R.toList . worldGen (R.ix2 500 1000)-}

{-main = do-}
    {-getArgsAndInitialize-}
    {-initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]-}
    {-world <- fmap (subGen (R.ix2 70 150)) newStdGen-}
    {-createWindow "Automata"-}
    {-displayCallback $= display [1]-}
    {-depthFunc       $= Just Less-}
    {-mainLoop-}
