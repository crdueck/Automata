{-# LANGUAGE FlexibleContexts #-}

import Bindings
import Control.Monad
import Data.Array.Repa
import Data.Array.Repa.Repr.Cursored
import Data.IORef
import Graphics.Rendering.OpenGL hiding (index)
import Graphics.UI.GLUT hiding (index)
import Simplex

type Region a = Array U DIM3 a
type World  a = Array C DIM3 (Region a)

type HeightMap = Array D DIM2 Float

f2GLf :: Float -> GLfloat
f2GLf = realToFrac

i2GLf :: Int -> GLfloat
i2GLf = fromIntegral

i2f :: Int -> Float
i2f = fromIntegral

worldGen :: DIM3 -> World Float
worldGen sh = makeCursored sh id addDim (computeUnboxedS . getRegion)
    where getRegion (Z :. i :. j :. k) =
              let start = Z :. i - 64 :. j - 64 :. k - 32
                  end   = Z :. i + 64 :. j + 64 :. k + 32
              in extract start end world
          world = noise 3 4 +^ noise 2 10 +^ noise 2 14
          noise octave freq = fromFunction sh $ \(Z :. x :. y :. z) ->
              harmonic3D octave freq (i2f x) (i2f y) (i2f z)

heightMap :: DIM2 -> HeightMap
heightMap sh = noise 3 20 +^ noise 3 3 +^ noise 3 6
    where noise octave freq = fromFunction sh $ \(Z :. x :. y) ->
              harmonic2D octave freq (i2f x) (i2f y)

_STRIDE :: Int
_STRIDE = 2

renderHeightMap :: HeightMap -> IO ()
renderHeightMap arr = do
    let Z :. x :. y = extent arr
    forM_ [0,_STRIDE..x] $ \i ->
        forM_ [0,_STRIDE..y] $ \j -> do
            renderVertex  i             j
            renderVertex  i            (j + _STRIDE)
            renderVertex (i + _STRIDE) (j + _STRIDE)
            renderVertex (i + _STRIDE)  j
    where renderVertex i j = do
              let h = f2GLf . max 0 $ index arr (ix2 i j)
              color  $ Color3 0 0 h
              vertex $ Vertex3 (i2GLf i) (10 * h) (i2GLf j)

main = print $ sumAllS $ heightMap (ix2 512 512)

{-main = do-}
    {-getArgsAndInitialize-}
    {-initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]-}
    {-initialWindowSize  $= Size 1080 720-}
    {-createWindow "Automata"-}

    {-zoom     <- newIORef (0.01   ::  GLfloat)-}
    {-angle    <- newIORef ((-20, 5) :: (GLfloat, GLfloat))-}
    {-position <- newIORef ((-0.8, -0.5) :: (GLfloat, GLfloat))-}

    {-let hMap = heightMap $ ix2 256 256-}

    {-displayCallback       $= display angle position zoom (renderHeightMap hMap)-}
    {-keyboardMouseCallback $= Just (keyboardMouse angle position zoom)-}
    {-idleCallback          $= Just (postRedisplay Nothing)-}
    {-reshapeCallback       $= Just (\s -> viewport $= (Position 0 0, s))-}
    {-depthFunc             $= Just Less-}
    {-mainLoop-}
