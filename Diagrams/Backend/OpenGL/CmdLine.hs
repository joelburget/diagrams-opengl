{-# LANGUAGE FlexibleContexts #-}
module Diagrams.Backend.OpenGL.CmdLine where

import Control.Monad.State
import Control.Monad.Task
import Diagrams.Prelude
import Graphics.Rendering.Cairo hiding (identityMatrix)
import Graphics.Rendering.Cairo as C
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW.Task
import Graphics.Rendering.Pango

import Diagrams.Backend.OpenGL.Internal
import Diagrams.Backend.OpenGL.Matrix
import Diagrams.Backend.OpenGL.Models
import Diagrams.Backend.OpenGL.Texture

data World = World
    { dirty  :: Bool
    , models :: [RetainedModel]
    , diagram :: Diagram OpenGL R2
    }

getDirty :: M Bool
getDirty = fmap dirty get

putDirty :: MonadState World m => Bool -> m ()
putDirty y = modify $ \x -> x { dirty = y }

type M a = TaskT Event (StateT World IO) a

buttonPress, buttonRelease :: Event -> Maybe GLFW.MouseButton 
buttonPress   (MouseButtonEvent b GLFW.Press)   = Just b
buttonPress   _                                 = Nothing
buttonRelease (MouseButtonEvent b GLFW.Release) = Just b
buttonRelease _                                 = Nothing

repeatUntil :: MonadTask a m => (a -> Maybe b) -> m c -> m b
repeatUntil f m = loop
  where loop = m >> watch Just >>= maybe loop return . f

set2DViewport :: GL.Size -> IO ()
set2DViewport size@(GL.Size w h) = do
  putStrLn "Setting 2d viewport"
  GL.viewport   $= (GL.Position 0 0, size)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

cairoTask :: World -> IO ()
-- cairoTask initial = (`evalStateT` initial) . runTask task where
cairoTask = evalStateT (runTask task) where
 -- task :: OpenGLRenderM ()
 task :: M ()
 task = do
  -- here the monad is of type M ()
  waitForEvents <- liftIO registerTaskCallbacks :: M (M ())
  fork $ forever $ watch onSize >>= liftIO . set2DViewport
  fork $ forever $ watch onRefresh >> putDirty True
  fork $ forever $ watch onClose >> exit
  -- fork $ forever   interaction
  putDirty True
  forever $ do
    waitForEvents
    liftIO $ putStrLn "dirty?"
    World d s dia <- get
    -- when d $ liftIO (putStrLn "here" >> drawSurfaces s >> GLFW.swapBuffers)
    -- when d $ liftIO (putStrLn "here" >> liftM fst (doRender OpenGL (OpenGLOptions (Width 400)) dia :: Result OpenGL R2) >> GLFW.swapBuffers)
    when d $ liftIO $ do
        putStrLn "here"
        fst $ renderDia OpenGL (OpenGLOptions (Width 400)) dia
        GLFW.swapBuffers
    putDirty False
    yield              -- give other tasks chance to run
  {-
  where
    interaction = do
      watch buttonPress
      (GL.Position x y) <- liftIO $ GL.get GLFW.mousePos
      modifyLines (((x, y):) . ((x, y):)) 
      repeatUntil buttonRelease $ do
        (GL.Position x' y') <- liftIO $ GL.get GLFW.mousePos
        modifyLines (((x', y'):) . tail)
        -}

buildWorld :: Diagram OpenGL R2 -> World
buildWorld = World True undefined

uncurry3 :: (a -> b -> c -> r) -> (a, b, c) -> r
uncurry3 f (a, b, c) = f a b c

textModel :: Int -> Int -> LayoutAlignment -> Markup -> IO RetainedModel
textModel x y layout = createTextTexture layout >=> uncurry3 (texModel x y)

pangoLayoutModel :: Int -> Int -> PangoLayout -> IO RetainedModel
pangoLayoutModel x y = createPangoLayoutTexture >=> uncurry3 (texModel x y)

texModel :: Int -> Int -> Int -> Int -> GL.TextureObject -> IO RetainedModel
texModel x y w h tex = do
    q <- getQuadImageModel (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
    return $ RetainedModel (q {textures = [tex]}) True

cairoModel :: Int -> Int -> Int -> Int -> C.Render () -> IO RetainedModel
cairoModel x y w h r = withImageSurface FormatARGB32 w h $ \s -> do
    renderWith s r
    tex <- createTexture GL.Texture2D GL.Disabled $
        texImage2DSurface Nothing 0 s
    texModel x y w h tex

defaultMain :: Diagram OpenGL R2 -> IO ()
defaultMain d = do
    GLFW.initialize
    -- open window
    GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayAlphaBits 8] GLFW.Window
    GLFW.windowTitle $= "GLFW Demo"
    GL.shadeModel    $= GL.Smooth
    -- enable antialiasing
    GL.lineSmooth $= GL.Enabled
    GL.blend      $= GL.Enabled
    GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    -- set the color to clear background
    GL.clearColor $= GL.Color4 0 0 0 0

    cairoTask $ buildWorld d
