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
    render = renderPrimitive Quads . renderHeightMap

{-instance Num a => Num (Vector3 a) where-}
    {-(Vector3 a b c) + (Vector3 x y z) = Vector3 (a + x) (b + y) (c + z)-}
    {-(Vector3 a b c) * (Vector3 x y z) = -- cross product-}
        {-Vector3 (b * z - c * y) (c * x - a * z) (a * y - b * x)-}
    {-fromInteger x = Vector3 (fromIntegral x) (fromIntegral x) (fromIntegral x)-}

data Camera = Camera
    { cameraPos :: Vector3 GLdouble
    , cameraRot :: Vector2 GLdouble
    }

f2GLf :: Float -> GLfloat
f2GLf = realToFrac

i2GLf :: Int -> GLfloat
i2GLf = fromIntegral

toRad :: Floating a => a -> a
toRad deg = deg * pi / 180

renderHeightMap :: HeightMap -> IO ()
renderHeightMap arr = do
    let delta = 1
        Z :. x :. y = extent arr
    forM_ [0,delta..x] $ \i ->
        forM_ [0,delta..y] $ \j -> do
            renderVertex  i           j
            renderVertex  i          (j + delta)
            renderVertex (i + delta) (j + delta)
            renderVertex (i + delta)  j
    where renderVertex i j = do
            let h = f2GLf . max 0 $ A.index arr (ix2 i j)
            color  $ Color3 0 0 h
            vertex $ Vertex3 (10 * i2GLf i) (30 * h) (10 * i2GLf (-j))

{-initFog :: IO ()-}
{-initFog = do-}
    {-hint Fog $= Nicest-}
    {-fogMode  $= Linear 0 1-}
    {-fogColor $= Color4 0.2 0.2 0.2 0.0-}
    {-fog      $= Enabled-}

{-initLighting :: IO ()-}
{-initLighting = do-}
    {-lighting           $= Enabled-}
    {-light (Light 0)    $= Enabled-}
    {-position (Light 0) $= Vertex4 0 5 10 1-}
    {-lightModelAmbient  $= Color4 0.2 0.2 0.2 1-}

initGL :: IO ()
initGL = do
    clearColor $= Color4 0.0 0.0 0.0 1.0
    cullFace   $= Just Front
    depthFunc  $= Just Less
    shadeModel $= Smooth
    hint PerspectiveCorrection $= Nicest

renderScene :: Renderable a => IORef Camera -> a -> IO ()
renderScene camera scene = do
    clear [ColorBuffer, DepthBuffer]
    matrixMode $= Projection
    loadIdentity
    perspective 45 1.5 1 1000
    matrixMode $= Modelview 0
    loadIdentity
    Camera pos (Vector2 rotX rotY) <- get camera
    {-rotate 20   $ Vector3 1 0 (0 :: GLdouble)-}
    rotate rotY $ Vector3 0 1 (0 :: GLdouble)
    translate pos
    render scene
    swapBuffers

myKeyCallback :: IORef Camera -> KeyCallback
myKeyCallback camera key state = do
    Camera (Vector3 dx dy dz) rot@(Vector2 rotX rotY) <- get camera
    when (state == Press) $ case key of
        CharKey c -> case c of
            '-' -> camera $= Camera (Vector3 dx (dy + 10) dz) rot
            '=' -> camera $= Camera (Vector3 dx (dy - 10) dz) rot

            'W' -> camera $= Camera (Vector3 (dx + 10 * sin (toRad rotY)) dy (dz + 10 * cos (toRad rotY))) rot
            'S' -> camera $= Camera (Vector3 (dx - 10 * sin (toRad rotY)) dy (dz - 10 * cos (toRad rotY))) rot
            'A' -> camera $= Camera (Vector3 (dx + 10) dy dz) rot
            'D' -> camera $= Camera (Vector3 (dx - 10) dy dz) rot
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
        dy = fromIntegral $ y - midX

    camera   $= Camera pos (Vector2 (rotX + dy) (rotY + dx))
    mousePos $= Position midX midY

main :: IO ()
main = do
    initialize
    openWindow (Size 1080 720) [DisplayDepthBits 32, DisplayRGBBits 8 8 8] Window
    windowTitle $= "Automata"
    enableSpecial KeyRepeat
    disableSpecial MouseCursor
    initGL

    camera <- newIORef $ Camera (Vector3 (-640) 10 (-340)) (Vector2 0 0)

    let world  = heightMap (ix2 128 128)

    windowSizeCallback $= \s -> viewport $= (Position 0 0, s)
    mousePosCallback   $= myMouseCallback camera
    keyCallback        $= myKeyCallback camera

    forever $ renderScene camera world
