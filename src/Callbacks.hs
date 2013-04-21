module Callbacks where

import Control.Monad
import Data.IORef
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import System.Exit

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
