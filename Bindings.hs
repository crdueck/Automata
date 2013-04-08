module Bindings where

import Data.IORef
import Data.Word
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

mkVert2 :: (Float, Float) -> IO ()
mkVert2 (x, y) = vertex $ Vertex2 x y

mkVert3 :: (Float, Float, Float) -> IO ()
mkVert3 (x, y, z) = vertex $ Vertex3 x y z

tile :: Float -> IO ()
tile w = renderPrimitive Quads $
    mapM_ mkVert2 [(w, w), (w,-w), (-w,-w), (-w,w)]

cube :: Float -> IO ()
cube w = renderPrimitive Quads $ mapM_ mkVert3
    [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w)
    , ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w)
    , ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w)
    , (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w)
    , ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w)
    , ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]

cubeFrame :: Float -> IO ()
cubeFrame w = renderPrimitive Lines $ mapM_ mkVert3
    [ ( w,-w, w), ( w, w, w), ( w, w, w), (-w, w, w)
    , (-w, w, w), (-w,-w, w), (-w,-w, w), ( w,-w, w)
    , ( w,-w, w), ( w,-w,-w), ( w, w, w), ( w, w,-w)
    , (-w, w, w), (-w, w,-w), (-w,-w, w), (-w,-w,-w)
    , ( w,-w,-w), ( w, w,-w), ( w, w,-w), (-w, w,-w)
    , (-w, w,-w), (-w,-w,-w), (-w,-w,-w), ( w,-w,-w) ]

grass :: Color3 Float
grass = Color3 0.0 10.0 0.0

stone :: Color3 Float
stone = Color3 3.0 3.0 3.0

chooseColor :: Word8 -> IO ()
chooseColor 0 = color grass
chooseColor _ = color stone

display :: [Word8] -> IO ()
display ws = do
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    mapM_ (\w -> chooseColor w >> tile (fromIntegral w)) ws
    swapBuffers
