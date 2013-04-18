{-# LANGUAGE FlexibleInstances #-}

import Control.Monad
import Data.Array.Repa
import Data.Array.Repa.Repr.Cursored
import Data.IORef
import Graphics.Rendering.OpenGL hiding (index)
import Graphics.UI.GLFW
import Simplex
import System.Exit

type HeightMap = Array D DIM2 Float
type Region a  = Array D DIM3 a
type World  a  = Array C DIM3 (Region a)

f2GLf :: Float -> GLfloat
f2GLf = realToFrac

i2GLf :: Int -> GLfloat
i2GLf = fromIntegral

i2f :: Int -> Float
i2f = fromIntegral

worldGen :: DIM3 -> World Float
worldGen sh = makeCursored sh id addDim getRegion
    where getRegion (Z :. i :. j :. k) =
              let start = Z :. i - 32 :. j - 32 :. k - 32
                  end   = Z :. i + 32 :. j + 32 :. k + 32
              in extract start end world
          world = regionGen sh

regionGen :: DIM3 -> Region Float
regionGen sh = noise 3 22 +^ noise 3 33 +^ noise 3 44
    where noise octave freq = fromFunction sh $ \(Z :. x :. y :. z) ->
            harmonic3D octave freq (i2f x) (i2f y) (i2f z)

heightMap :: DIM2 -> HeightMap
heightMap sh = noise 3 10 +^ noise 3 15 +^ noise 3 20 +^ noise 3 3 +^ noise 3 0.25
    where noise octave freq = fromFunction sh $ \(Z :. x :. y) ->
              harmonic2D octave freq (i2f x) (i2f y)

class Renderable a where
    render :: a -> IO ()

instance Renderable HeightMap where
    render = renderPrimitive Quads . renderHeightMap

renderHeightMap :: HeightMap -> IO ()
renderHeightMap arr = do
    let delta = 2
        Z :. x :. y = extent arr
    forM_ [0,delta..x] $ \i ->
        forM_ [0,delta..y] $ \j -> do
            renderVertex  i           j
            renderVertex  i          (j + delta)
            renderVertex (i + delta) (j + delta)
            renderVertex (i + delta)  j
    where renderVertex i j = do
              let h = max 0 $ index arr (ix2 i j)
              color  $ Color3 0 0 h
              vertex $ Vertex3 (10 * i2GLf i) (30 * h) (10 * i2GLf (-j))

initFog :: IO ()
initFog = do
    hint Fog $= DontCare
    fogMode  $= Linear 0 1
    fogColor $= Color4 0.2 0.2 0.2 0.0
    fog      $= Enabled

initLighting :: IO ()
initLighting = do
    lighting           $= Enabled
    light (Light 0)    $= Enabled
    position (Light 0) $= Vertex4 0 5 10 1
    lightModelAmbient  $= Color4 0.2 0.2 0.2 1

initGL :: IO ()
initGL = do
    clearColor $= Color4 0.1 0.1 0.1 1.0
    cullFace   $= Just Front
    depthFunc  $= Just Less
    shadeModel $= Smooth
    hint PerspectiveCorrection $= DontCare

initGLFW :: IO ()
initGLFW = do
    enableSpecial KeyRepeat

renderScene :: Renderable a => IORef (Vertex3 GLdouble) -> a -> IO ()
renderScene camera scene = do
    clear [ColorBuffer, DepthBuffer]
    matrixMode $= Projection
    loadIdentity
    cameraPos <- get camera
    perspective 45 1.5 1 1000
    lookAt cameraPos (Vertex3 640 0 (-640)) (Vector3 0 1 0)
    render scene
    swapBuffers

myKeyCallback :: IORef (Vertex3 GLdouble) -> KeyCallback
myKeyCallback camera key state = do
    Vertex3 dx dy dz <- get camera
    when (state == Press) $ case key of
        CharKey c    -> case c of
            '-'   -> camera $= Vertex3 dx (dy - 10) dz
            '='   -> camera $= Vertex3 dx (dy + 10) dz
            _     -> return ()
        SpecialKey k -> case k of
            ESC   -> closeWindow >> terminate >> exitSuccess
            UP    -> camera $= Vertex3 dx dy (dz - 10)
            DOWN  -> camera $= Vertex3 dx dy (dz + 10)
            LEFT  -> camera $= Vertex3 (dx - 10) dy dz
            RIGHT -> camera $= Vertex3 (dx + 10) dy dz
            _     -> return ()

myMousePosCallback :: IORef (Vertex3 GLdouble) -> MousePosCallback
myMousePosCallback _ (Position x y) = return ()

main :: IO ()
main = do
    initialize
    openWindow (Size 1080 720) [DisplayDepthBits 32, DisplayRGBBits 8 8 8] Window
    windowTitle $= "Automata"
    initGL
    initGLFW

    let world = heightMap (ix2 128 128)
    camera <- newIORef (Vertex3 64 64 64)
    eye    <- newIORef (Vertex3 64 0 (-64))

    windowSizeCallback $= \s -> viewport $= (Position 0 0, s)
    {-mousePosCallback   $= myMousePosCallback eye-}
    keyCallback        $= myKeyCallback camera

    forever $ renderScene camera world
