module Bindings where

import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

display angle position zoom renderIO = do
    clear [DepthBuffer, ColorBuffer]
    loadIdentity
    scalef <- get zoom
    (a, b) <- get angle
    (x, y) <- get position
    rotate a  $ Vector3 1 0 0
    rotate b  $ Vector3 0 1 0
    translate $ Vector3 x 0 y
    scale scalef scalef scalef
    renderPrimitive Quads renderIO
    swapBuffers

keyboardMouse angle position zoom key state _ _ =
    when (state == Down) $ do
        scalef <- get zoom
        (a, b) <- get angle
        (x, y) <- get position
        case key of
            SpecialKey KeyLeft  -> position $= (x - 0.1, y)
            SpecialKey KeyRight -> position $= (x + 0.1, y)
            SpecialKey KeyUp    -> position $= (x, y + 0.1)
            SpecialKey KeyDown  -> position $= (x, y - 0.1)
            Char 'h' -> angle $= (a - 1, b)
            Char 'l' -> angle $= (a + 1, b)
            Char 'j' -> angle $= (a, b - 1)
            Char 'k' -> angle $= (a, b + 1)
            Char '-' -> zoom  $= max (scalef - 0.01) 0.01
            Char '+' -> zoom  $= min (scalef + 0.01) 1
            _        -> return ()
