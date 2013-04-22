{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Callbacks
import Control.Monad
import Data.Array.Repa as A
import Data.IORef
import qualified Data.Set as S
import qualified Data.Vector.Storable as V
import Foreign.Storable
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import WorldGen

class Renderable a where
    render :: a -> IO ()

instance Renderable Region where
    render = renderRegion

initFog :: IO ()
initFog = do
    fog      $= Enabled
    fogMode  $= Linear 250 2000
    fogColor $= Color4 0.2 0.2 0.2 1.0
    hint Fog $= Nicest

initLighting :: IO ()
initLighting = do
    lighting           $= Enabled
    light (Light 0)    $= Enabled
    position (Light 0) $= Vertex4 0 5 10 1
    lightModelAmbient  $= Color4 0.2 0.2 0.2 1

initialize' :: IO ()
initialize' = do
    clearColor $= Color4 0.5 0.7 1.0 1.0
    cullFace   $= Just Back
    depthFunc  $= Just Less
    lineSmooth $= Enabled
    shadeModel $= Smooth
    hint PerspectiveCorrection $= Nicest
    initFog

toVertexBuffer :: (Num e, Shape sh, Source r e) => Array r sh e -> [e]
toVertexBuffer arr = concat . toList . traverse arr id $ \f ix ->
    f ix : fmap fromIntegral (listOfShape ix)

makeBuffer :: forall a. Storable a => BufferTarget -> [a] -> IO BufferObject
makeBuffer target elems = do
    [buffer] <- genObjectNames 1
    bindBuffer target $= Just buffer
    let v = V.fromList elems
        n = fromIntegral $ V.length v * sizeOf (undefined :: a)
    V.unsafeWith v $ \ptr -> bufferData target $= (n, ptr, StaticDraw)
    return buffer

renderRegion :: Region -> IO ()
renderRegion arr = do
    let Z :. x :. y = extent arr
    forM_ [(i, j) | i <- [0..x-2], j <- [0..y-2]] $ \(i, j) ->
        renderPrimitive LineStrip $ do
            renderVertex  i       j
            renderVertex  i      (j + 1)
            renderVertex (i + 1)  j
            renderVertex (i + 1) (j + 1)
    where renderVertex i j = do
            let h = realToFrac . max 0 $ A.index arr (ix2 i j) :: GLfloat
            color  $ if h == 0 then Color3 0.0 0.75 0.33 else Color3 0 0 h
            vertex $ fmap (*10) $ Vertex3 (fromIntegral i) (10 * h) (fromIntegral j)

renderWorld :: Renderable a => IORef Camera -> IORef Model -> Double -> a -> IO ()
renderWorld camera model t0 world = do
    clear [ColorBuffer, DepthBuffer]
    matrixMode $= Projection
    loadIdentity
    perspective 45 1.5 1 10000
    matrixMode $= Modelview 0
    loadIdentity
    Camera pos (rotX, rotY) <- get camera
    rotate rotX $ Vector3 1 0 (0 :: GLdouble)
    rotate rotY $ Vector3 0 1 (0 :: GLdouble)
    translate pos
    render world
    swapBuffers

updateWorld :: Renderable a => IORef Camera -> IORef Model -> Double -> a -> IO ()
updateWorld camera model t0 world = do
    Camera (Vector3 x y z) rot@(_, rotY') <- get camera
    Model (i, j) pressedKeys <- get model

    t1 <- get time
    let dt   = realToFrac $ 1000 * (t1 - t0)
        rotY = deg2rad rotY'

        handleKey key (dx, dy, dz) = case key of
            CharKey 'W' -> (dx - dt * sin rotY, dy, dz + dt * cos rotY)
            CharKey 'S' -> (dx + dt * sin rotY, dy, dz - dt * cos rotY)
            CharKey 'A' -> (dx + dt * cos rotY, dy, dz + dt * sin rotY)
            CharKey 'D' -> (dx - dt * cos rotY, dy, dz - dt * sin rotY)
            CharKey '-' -> (dx, dy + dt, dz)
            CharKey '=' -> (dx, dy - dt, dz)
            _ -> (dx, dy, dz)

        (dx, dy, dz) = S.foldr handleKey (0, 0, 0) pressedKeys
        newPos = Vector3 (x + dx) (y + dy) (z + dz)

    camera $= Camera newPos rot

main :: IO ()
main = do
    initialize
    openWindow (Size 1080 720) [DisplayDepthBits 32, DisplayRGBBits 8 8 8] Window
    windowTitle  $= "Automata"
    swapInterval $= 1
    enableSpecial  KeyRepeat
    disableSpecial MouseCursor
    initialize'

    let r = regionGen (ix2 256 256)
        w = worldGen  (ix2 256 256)

    camera <- newIORef $ Camera (Vector3 0 (-10) 0) (0, 0)
    model  <- newIORef $ Model (0, 0) S.empty

    windowSizeCallback $= \s -> viewport $= (Position 0 0, s)
    mousePosCallback   $= myMouseCallback camera
    keyCallback        $= myKeyCallback model

    forever $ do
        t0 <- get time
        renderWorld camera model t0 r
        updateWorld camera model t0 r
