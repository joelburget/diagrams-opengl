{-# LANGUAGE GADTs, RankNTypes, FlexibleContexts #-}
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Task
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.Cairo hiding (identityMatrix)
import Prelude hiding (lines)
import Graphics.UI.GLFW.Task
import Diagrams.Backend.OpenGL.Matrix
import Diagrams.Backend.OpenGL.Texture
import Diagrams.Backend.OpenGL.Models
import Graphics.Rendering.Pango

buttonPress, buttonRelease :: Event -> Maybe GLFW.MouseButton
buttonPress   (MouseButtonEvent b GLFW.Press)   = Just b
buttonPress   _                                 = Nothing
buttonRelease (MouseButtonEvent b GLFW.Release) = Just b
buttonRelease _                                 = Nothing

repeatUntil :: MonadTask a m => (a -> Maybe b) -> m c -> m b
repeatUntil f m = loop
  where loop = m >> watch Just >>= maybe loop return . f

data RetainedModel = RetainedModel
    { model    :: Model --GL.TextureObject
    , modified :: Bool
    } deriving Show

data World = World
    { dirty  :: Bool
    , models :: [RetainedModel]
    } deriving Show

type M a = TaskT Event (StateT World IO) a

getDirty :: M Bool
getDirty = fmap dirty get

putDirty :: MonadState World m => Bool -> m ()
putDirty y = modify $ \x -> x { dirty = y }

getSurfaces :: M [RetainedModel]
getSurfaces = fmap models get

set2DViewport :: GL.Size -> IO ()
set2DViewport size@(GL.Size w h) = do
  putStrLn "Setting 2d viewport"
  GL.viewport   $= (GL.Position 0 0, size)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

cairoTask :: World -> IO ()
cairoTask initial = (`evalStateT` initial) . runTask $ do
  -- here the monad is of type M ()
  waitForEvents <- liftIO registerTaskCallbacks
  fork $ forever $ watch onSize >>= liftIO . set2DViewport
  fork $ forever $ watch onRefresh >> putDirty True
  fork $ forever $ watch onClose >> exit
  -- fork $ forever   interaction
  putDirty True
  forever $ do
    waitForEvents
    liftIO $ putStrLn "dirty?"
    d <- getDirty
    s <- getSurfaces
    -- when d $ getLines >>= liftIO . drawLines >> liftIO GLFW.swapBuffers
    when d $ liftIO $ do
        putStrLn "here"
        drawSurfaces s
        print s
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
    -- set the color to clear background
    GL.clearColor $= GL.Color4 0 0 0 0

    initial <- sequence [smiley] --, boxes, helloWorld]
    --initial <- sequence [boxes, smiley]

    cairoTask $ World True initial

w, h :: Int
w = 400
h = 400

drawSurfaces :: [RetainedModel] -> IO ()
drawSurfaces models = forM_ models $ \m -> when (modified m) $ do
    putStrLn "drawing Model"
    drawModel $ model m

boxes :: IO RetainedModel
boxes = cairoModel 0 0 w h $ do
    setLineWidth 1
    setSourceRGB 1 1 0
    rectangle 100 100 100 100
    rectangle 200 200 100 100
    rectangle 300 300 100 100
    rectangle 0 0 400 400
    moveTo 0 0
    lineTo 400 400
    stroke

smiley :: IO RetainedModel
smiley = cairoModel 0 0 w h $ do
    let w' = fromIntegral w
        h' = fromIntegral h
    setSourceRGB 1 1 1
    paint

    setSourceRGB 0 0 0
    moveTo 0 0
    lineTo w' h'
    moveTo w' 0
    lineTo 0 h'
    setLineWidth (0.1 * (h' + w'))
    stroke

    rectangle 0 0 (0.5 * w') (0.5 * h')
    setSourceRGBA 1 0 0 0.8
    fill

    rectangle 0 (0.5 * h') (0.5 * w') (0.5 * h')
    setSourceRGBA 0 1 0 0.6
    fill

    rectangle (0.5 * w') 0 (0.5 * w') (0.5 * h')
    setSourceRGBA 0 0 1 0.4
    fill

{-
helloWorld :: IO RetainedModel
helloWorld = textModel 200 200 AlignCenter "Hello World!"

uncurry3 :: (a -> b -> c -> r) -> (a, b, c) -> r
uncurry3 f (a, b, c) = f a b c

textModel :: Int -> Int -> LayoutAlignment -> Markup -> IO RetainedModel
textModel x y layout = createTextTexture layout >=> uncurry3 (texModel x y)

pangoLayoutModel :: Int -> Int -> PangoLayout -> IO RetainedModel
pangoLayoutModel x y = createPangoLayoutTexture >=> uncurry3 (texModel x y)
-}

texModel :: Int -> Int -> Int -> Int -> GL.TextureObject -> IO RetainedModel
texModel x y w h tex = do
    q <- getQuadImageModel (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
    return $ RetainedModel (q {textures = [tex]}) True

cairoModel :: Int -> Int -> Int -> Int -> Render () -> IO RetainedModel
cairoModel x y w h r = withImageSurface FormatARGB32 w h $ \s -> do
    renderWith s r
    tex <- createTexture GL.Texture2D GL.Disabled $ texImage2DSurface Nothing 0 s
    texModel x y w h tex
