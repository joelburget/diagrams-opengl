{-# LANGUAGE GADTs, RankNTypes, FlexibleContexts #-}
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Task
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Prelude hiding (lines)
import Graphics.UI.GLFW.Task

type Lines = [(GL.GLint, GL.GLint)]

drawLines :: Lines -> IO ()
drawLines lines = do
  GL.clear [GL.ColorBuffer]
  GL.color $ GL.Color3 1 0 (0::GL.GLdouble)
  GL.renderPrimitive GL.Lines $ mapM_ 
    (\ (x, y) -> GL.vertex (GL.Vertex3 (fromIntegral x) 
                                       (fromIntegral y) 
                                       (0::GL.GLdouble))) lines

buttonPress, buttonRelease :: Event -> Maybe GLFW.MouseButton 
buttonPress   (MouseButtonEvent b GLFW.Press)   = Just b
buttonPress   _                                 = Nothing
buttonRelease (MouseButtonEvent b GLFW.Release) = Just b
buttonRelease _                                 = Nothing

repeatUntil :: MonadTask a m => (a -> Maybe b) -> m c -> m b
repeatUntil f m = loop
  where loop = m >> watch Just >>= maybe loop return . f

data S = S { lines :: Lines, dirty :: Bool }
type M a = TaskT Event (StateT S IO) a

getLines :: TaskT Event (StateT S IO) Lines
getLines      = fmap lines get

modifyLines :: MonadState S m => (Lines -> Lines) -> m ()
modifyLines f = modify $ \x -> x { lines = f (lines x), dirty = True }

getDirty :: TaskT Event (StateT S IO) Bool
getDirty      = fmap dirty get

putDirty :: MonadState S m => Bool -> m ()
putDirty y    = modify $ \x -> x { dirty = y }

set2DViewport :: GL.Size -> IO ()
set2DViewport size@(GL.Size w h) = do
  GL.viewport   $= (GL.Position 0 0, size)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

lineTask :: IO ()
lineTask = (`evalStateT` S [] False) . runTask $ do
  -- here the monad is of type M ()
  waitForEvents <- liftIO registerTaskCallbacks
  fork $ forever $ watch onSize >>= liftIO . set2DViewport
  fork $ forever $ watch onRefresh >> putDirty True
  fork $ forever $ watch onClose >> exit
  fork $ forever   interaction
  forever $ do
    waitForEvents
    d <- getDirty
    when d $ getLines >>= liftIO . drawLines >> liftIO GLFW.swapBuffers
    putDirty False
    yield              -- give other tasks chance to run
  where
    interaction = do
      watch buttonPress
      (GL.Position x y) <- liftIO $ GL.get GLFW.mousePos
      modifyLines (((x, y):) . ((x, y):)) 
      repeatUntil buttonRelease $ do
        (GL.Position x' y') <- liftIO $ GL.get GLFW.mousePos
        modifyLines (((x', y'):) . tail)

main :: IO ()
main = do
    GLFW.initialize
    -- open window
    GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayAlphaBits 8] GLFW.Window
    GLFW.windowTitle $= "GLFW Demo"
    GL.shadeModel    $= GL.Smooth
    -- enable antialiasing
    GL.lineSmooth $= GL.Enabled
    GL.blend      $= GL.Enabled
    GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.lineWidth  $= 1.5
    -- set the color to clear background
    GL.clearColor $= GL.Color4 0 0 0 0
   
    -- set 2D orthogonal view inside windowSizeCallback because
    -- any change to the Window size should result in different
    -- OpenGL Viewport.
    GLFW.windowSizeCallback $= set2DViewport
    lineTask
    -- -- keep all line strokes as a list of points in an IORef
    -- lines <- newIORef []
    -- -- run the main loop
    -- run lines
    -- finish up
    GLFW.closeWindow
    GLFW.terminate
