module Callbacks where

import Data.IORef
import qualified Data.Set as S
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import System.Exit

data Camera = Camera (Vector3 GLdouble) (GLdouble, GLdouble)
data Model  = Model (Int, Int) (S.Set Key)

myKeyCallback :: IORef Model -> KeyCallback
myKeyCallback model (SpecialKey ESC) Press =
    closeWindow >> terminate >> exitSuccess
myKeyCallback model key state = do
    Model regionIx pressedKeys <- get model
    if state == Press
       then model $= Model regionIx (S.insert key pressedKeys)
       else model $= Model regionIx (S.delete key pressedKeys)

myMouseCallback :: IORef Camera -> MousePosCallback
myMouseCallback camera (Position x y) = do
    Camera pos (rotX, rotY) <- get camera
    Size w h <- get windowSize

    let midX = w `quot` 2
        midY = h `quot` 2
        dx = fromIntegral $ x - midX
        dy = fromIntegral $ y - midY

    camera   $= Camera pos (clamp (-90) 90 (rotX + dy), (roll (-180) 180 (rotY + dx)))
    mousePos $= Position midX midY

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
