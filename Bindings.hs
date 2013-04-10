module Bindings where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

data Model = Model
    { mPosition :: {-# UNPACK #-} !(Vector3 GLfloat)
    , mRotation :: {-# UNPACK #-} !(Vector3 GLfloat)
    , mZoom     :: {-# UNPACK #-} !(Vector3 GLfloat)
    }

cube :: (GLfloat, GLfloat, GLfloat) -> IO ()
cube (x, y, z) = do
    clear [DepthBuffer, ColorBuffer]
    loadIdentity
    translate $ Vector3 x y z
    color stone
    renderObject Solid (Cube 1.0)
    swapBuffers

chooseTexture :: (Eq a, Num a) => a -> IO ()
chooseTexture 0 = color stone
chooseTexture _ = color grass

renderCube (x, y, z) w = do
    chooseTexture w
    renderObject Solid (Cube 0.2)

renderTile ((x', y'), w) = do
    chooseTexture w
    renderPrimitive Quads $ mapM_ vertex
        [ Vertex2 (x-n) (y-n), Vertex2 (x+n) (y-n)
        , Vertex2 (x+n) (y+n), Vertex2 (x-n) (y+n)]
    where n = 0.8 :: GLfloat
          x = fromIntegral x'
          y = fromIntegral y'

display angle xs = do
    clear [DepthBuffer, ColorBuffer]
    loadIdentity
    {-let pos = Vertex3 0 0 (10 :: GLdouble)-}
        {-aim = Vertex3 0 0 (0 :: GLdouble)-}
        {-upv = Vector3 0 1 (0 :: GLdouble)-}
    {-lookAt pos aim upv-}
    (x, y) <- get angle
    rotate x $ Vector3 1.0 0.0 0.0
    rotate y $ Vector3 0.0 1.0 0.0
    scale 0.02 0.02 (0.02 :: GLfloat)
    preservingMatrix $ mapM_ renderTile xs
    swapBuffers

grass :: Color3 GLfloat
grass = Color3 0.0 0.7 0.0

stone :: Color3 GLfloat
stone = Color3 0.2 0.2 0.2

reshape s = viewport $= (Position 0 0, s)

keyboardMouse angle key state modifiers position = do
    (x, y) <- get angle
    if state == Down
        then case key of
            Char 'h' -> angle $= (x + 0.5, y)
            Char 'l' -> angle $= (x - 0.5, y)
            Char 'j' -> angle $= (x, y + 0.5)
            Char 'k' -> angle $= (x, y - 0.5)
            _        -> return ()
        else return ()
