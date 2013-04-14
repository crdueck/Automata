import Bindings
import Control.Monad
import Data.Array.Repa
import Data.Array.Repa.Repr.Cursored
import Data.IORef
import Graphics.Rendering.OpenGL
{-import Graphics.UI.GLFW-}
import Simplex
import System.Exit

type Region a = Array D DIM3 a
type World  a = Array C DIM3 (Region a)

type HeightMap = Array D DIM2 Float

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
heightMap sh = noise 3 48 +^ noise 3 32 +^ noise 3 48
    where noise octave freq = fromFunction sh $ \(Z :. x :. y) ->
              harmonic2D octave freq (i2f x) (i2f y)

{-renderHeightMap :: HeightMap -> IO ()-}
{-renderHeightMap arr = do-}
    {-let Z :. x :. y = extent arr-}
          {-_STRIDE = 3-}
    {-forM_ [0,_STRIDE..x] $ \i ->-}
        {-forM_ [0,_STRIDE..y] $ \j -> do-}
            {-renderVertex  i             j-}
            {-renderVertex  i            (j + _STRIDE)-}
            {-renderVertex (i + _STRIDE) (j + _STRIDE)-}
            {-renderVertex (i + _STRIDE)  j-}
    {-where renderVertex i j = do-}
              {-let h = f2GLf . max 0 $ index arr (ix2 i j)-}
              {-color  $ Color3 (h + 0.01) h h-}
              {-vertex $ Vertex3 (i2GLf (2 * i)) (30 * h) (i2GLf (2 * j))-}

{-initFog :: IO ()-}
{-initFog = do-}
    {-hint Fog $= Nicest-}
    {-fogMode  $= Exp2 0.5-}
    {-fogColor $= Color4 0.2 0.2 0.2 1.0-}
    {-fog      $= Enabled-}

{-initGL :: IO ()-}
{-initGL = do-}
    {-clearColor $= Color4 0.8 0.8 0.8 1.0-}
    {-cullFace   $= Just Front-}
    {-depthFunc  $= Just Less-}
    {-hint PerspectiveCorrection $= Nicest-}
    {-initFog-}

main = print $ harmonic3D (10^4) 10 10 10 10

{-main :: IO ()-}
{-main = do-}
    {-initialize-}
    {-openWindow (Size 1080 720) [DisplayDepthBits 32, DisplayRGBBits 8 8 8] Window-}
    {-windowTitle $= "Automata"-}
    {-initGL-}
    {-forever $ do-}
        {-quit <- fmap (==Press) $ getKey ESC-}
        {-when quit $ closeWindow >> terminate >> exitSuccess-}
        {-clear [ColorBuffer, DepthBuffer]-}
        {--- apply transformations-}
        {--- draw things-}
        {-swapBuffers-}

{-
zoom     <- newIORef (1.0 :: GLfloat)
angle    <- newIORef ((0, 0) :: (GLfloat, GLfloat))
position <- newIORef ((0.0, 0.0, 0.0) :: (GLfloat, GLfloat, GLfloat))
displayCallback       $= display angle position zoom (renderHeightMap $ heightMap $ ix2 256 256)
keyboardMouseCallback $= Just (keyboardMouse angle position zoom)
idleCallback          $= Just (postRedisplay Nothing)
reshapeCallback       $= Just (\s -> viewport $= (Position 0 0, s))
-}
