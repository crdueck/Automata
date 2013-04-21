{-# LANGUAGE FlexibleInstances #-}

import Control.Monad
import Data.Array.Repa as A
import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import System.Exit
import WorldGen

class Renderable a where
    render :: a -> IO ()

instance Renderable HeightMap where
    render = renderPrimitive Triangles . renderHeightMap

data Camera = Camera (Vector3 GLdouble) (Vector2 GLdouble)

rad2deg :: Floating a => a -> a
{-# INLINE rad2deg #-}
rad2deg rad = rad * 180 / pi

deg2rad :: Floating a => a -> a
{-# INLINE deg2rad #-}
deg2rad deg = deg * pi / 180

clamp :: Ord a => a -> a -> a -> a
{-# INLINE clamp #-}
clamp lo hi x = min (max x lo) hi

roll :: Ord a => a -> a -> a -> a
{-# INLINE roll #-}
roll lo hi x
    | x < lo = hi
    | x > hi = lo
    | otherwise = x

renderHeightMap :: HeightMap -> IO ()
renderHeightMap arr = do
    let Z :. x :. y = extent arr
    forM_ [0..x-2] $ \i ->
        forM_ [0..y-2] $ \j -> do
            renderVertex  i       j
            renderVertex (i + 1)  j
            renderVertex  i      (j + 1)
            renderVertex  i      (j + 1)
            renderVertex (i + 1)  j
            renderVertex (i + 1) (j + 1)
    where renderVertex i j = do
              let h = realToFrac . max 0 $ A.index arr (ix2 i j) :: GLfloat
              color  $ Color3 0 0 h
              vertex $ Vertex3 (fromIntegral i) (3 * h) (fromIntegral (-j))

initFog :: IO ()
initFog = do
    hint Fog $= Nicest
    fogMode  $= Linear 20 100
    fogColor $= Color4 0.2 0.2 0.2 1.0
    fog      $= Enabled

initLighting :: IO ()
initLighting = do
    lighting           $= Enabled
    light (Light 0)    $= Enabled
    position (Light 0) $= Vertex4 0 5 10 1
    lightModelAmbient  $= Color4 0.2 0.2 0.2 1

initGL :: IO ()
initGL = do
    clearColor $= Color4 0.0 0.0 0.0 1.0
    cullFace   $= Just Back
    depthFunc  $= Just Less
    shadeModel $= Smooth
    hint PerspectiveCorrection $= Nicest
    initFog

renderScene :: Renderable a => IORef Camera -> a -> IO ()
renderScene camera scene = do
    clear [ColorBuffer, DepthBuffer]
    matrixMode $= Projection
    loadIdentity
    perspective 45 1.5 1 1000
    matrixMode $= Modelview 0
    loadIdentity
    Camera pos (Vector2 rotX rotY) <- get camera
    rotate rotX $ Vector3 1 0 (0 :: GLdouble)
    rotate rotY $ Vector3 0 1 (0 :: GLdouble)
    translate pos
    render scene
    swapBuffers

myKeyCallback :: IORef Camera -> KeyCallback
myKeyCallback camera key state = do
    Camera (Vector3 x y z) rot@(Vector2 _ rotY') <- get camera
    let rotY = deg2rad rotY'
    when (state == Press) $ case key of
        CharKey c -> case c of
            '-' -> camera $= Camera (Vector3 x (y + 1) z) rot
            '=' -> camera $= Camera (Vector3 x (y - 1) z) rot
            'W' -> camera $= Camera (Vector3 (x - sin rotY) y (z + cos rotY)) rot
            'S' -> camera $= Camera (Vector3 (x + sin rotY) y (z - cos rotY)) rot
            'A' -> camera $= Camera (Vector3 (x + cos rotY) y (z + sin rotY)) rot
            'D' -> camera $= Camera (Vector3 (x - cos rotY) y (z - sin rotY)) rot
            _   -> return ()
        SpecialKey k -> case k of
            ESC -> closeWindow >> terminate >> exitSuccess
            _   -> return ()

myMouseCallback :: IORef Camera -> MousePosCallback
myMouseCallback camera (Position x y) = do
    Camera pos (Vector2 rotX rotY) <- get camera
    Size w h <- get windowSize

    let midX = w `quot` 2
        midY = h `quot` 2
        dx = fromIntegral $ x - midX
        dy = fromIntegral $ y - midY

    camera   $= Camera pos (Vector2 (clamp (-90) 90 $ rotX + dy) (roll (-180) 180 $ rotY + dx))
    mousePos $= Position midX midY

main :: IO ()
main = do
    initialize
    openWindow (Size 1080 720) [DisplayDepthBits 32, DisplayRGBBits 8 8 8] Window
    windowTitle $= "Automata"
    enableSpecial  KeyRepeat
    disableSpecial MouseCursor
    initGL

    camera <- newIORef $ Camera (Vector3 (-64) (-5) (-34)) (Vector2 0 0)

    let world  = heightMap (ix2 128 128)

    windowSizeCallback $= \s -> viewport $= (Position 0 0, s)
    mousePosCallback   $= myMouseCallback camera
    keyCallback        $= myKeyCallback camera

    forever $ renderScene camera world
