module Bindings where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

data Model = Model
    { mPosition :: {-# UNPACK #-} !(Vector3 GLfloat)
    , mRotation :: {-# UNPACK #-} !(Vector3 GLfloat)
    }

mkVert2 :: (GLfloat, GLfloat) -> IO ()
mkVert2 (x, y) = vertex $ Vertex2 x y

mkVert3 :: (GLfloat, GLfloat, GLfloat) -> IO ()
mkVert3 (x, y, z) = vertex $ Vertex3 x y z

tile :: (GLfloat, GLfloat) -> IO ()
tile (x, y) = do
    clear [DepthBuffer, ColorBuffer]
    loadIdentity
    color stone
    let verts = [(x-n, y-n), (x+n, y-n), (x+n, y+n), (x-n, y+n)]
        n = 0.25
    renderPrimitive Quads $ mapM_ mkVert2 verts
    swapBuffers

cube :: (GLfloat, GLfloat, GLfloat) -> IO ()
cube (x, y, z) = do
    clear [DepthBuffer, ColorBuffer]
    loadIdentity
    translate $ Vector3 x y z
    color stone
    renderObject Solid (Cube 1.0)
    swapBuffers

display :: Model -> DisplayCallback
display model = do
    matrixMode $= Projection
    translate $ mPosition model
    rotate 20 $ Vector3 1 0 (1 :: GLfloat)
    cube (0.0, 0.0, 0.0)
    matrixMode $= Modelview 0

grass :: Color3 GLfloat
grass = Color3 0.0 0.7 0.0

stone :: Color3 GLfloat
stone = Color3 0.2 0.2 0.2

reshape s = do
    viewport   $= (Position 0 0, s)
    matrixMode $= Projection
    loadIdentity
    ortho (-2.0) 1.0 (-1.0) 1.0 (-1.0) 1.0
    matrixMode $= Modelview 0

{-display :: [((Int, Int), Word8)] -> IO ()-}
{-display iws = do-}
    {-clear [ColorBuffer, DepthBuffer]-}
    {-loadIdentity-}
    {-mapM_ (\((x, y), w) -> chooseColor w >> tile x y) iws-}
    {-swapBuffers-}
