{-# LANGUAGE FlexibleContexts #-}
module Diagrams.Backend.OpenGL.CmdLine (
    interactiveMain,
    defaultMain,
    Interaction,
    Input(..)
    ) where

import Data.IORef
import Control.Monad
import Diagrams.Prelude
import Diagrams.TwoD
import Graphics.Rendering.Cairo hiding (identityMatrix)
import Graphics.Rendering.Cairo as C
import Graphics.Rendering.OpenGL (($=), get)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.Pango
import System.Exit as E

import Diagrams.Backend.OpenGL.Internal
import Diagrams.Backend.OpenGL.Matrix
import Diagrams.Backend.OpenGL.Models
import Diagrams.Backend.OpenGL.Texture

data Input = Input
    { mouseX :: Int
    , mouseY :: Int
    , time :: Double
    } deriving Show

type Interaction = Input -> Diagram OpenGL R2

set2DViewport :: IORef SizeSpec2D -> GL.Size -> IO ()
set2DViewport sizeSpec size@(GL.Size w h) = do
  GL.viewport   $= (GL.Position 0 0, size)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho2D 0 (realToFrac w) (realToFrac h) 0
  writeIORef sizeSpec $ Dims (realToFrac w) (realToFrac h)

cairoTask :: Interaction -> IO ()
cairoTask interact = do
  GLFW.disableSpecial GLFW.AutoPollEvent
  dirty <- newIORef True
  quit <- newIORef False
  input <- newIORef $ Input 0 0 0
  sizeSpec <- newIORef $ Width 400

  -- Window Callbacks
  GLFW.windowSizeCallback $= set2DViewport sizeSpec
  GLFW.windowRefreshCallback $= writeIORef dirty True
  GLFW.windowCloseCallback $= (writeIORef quit True >> return True)

  -- Input Callbacks
  GLFW.keyCallback $= \k s -> do
      when (fromEnum k == fromEnum GLFW.ESC && s == GLFW.Press) $
          writeIORef quit True

  GLFW.mousePosCallback $= \(GL.Position x y) -> do
      let cast = fromInteger . toInteger
      modifyIORef input $ \input' -> input' {mouseX = cast x, mouseY = cast y}
      writeIORef dirty True

  -- Main loop
  forever $ do
      GLFW.waitEvents

      quit' <- readIORef quit
      when quit' E.exitSuccess

      dirty' <- readIORef dirty
      when dirty' $ do
          input' <- readIORef input
          time' <- get GLFW.time
          sizeSpec' <- readIORef sizeSpec
          let dia = interact (input' {time = time'})
          retainedModel <- renderDia OpenGL (OpenGLOptions sizeSpec') dia
          drawSurfaces [retainedModel]
          GLFW.swapBuffers
      dirty $= False

drawSurfaces :: [RetainedModel] -> IO ()
drawSurfaces models = do
    GL.clear [GL.ColorBuffer]
    forM_ models $ \m -> when (modified m) $ drawModel $ model m

interactiveMain :: Interaction -> IO ()
interactiveMain interact = do
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

    cairoTask interact

defaultMain :: Diagram OpenGL R2 -> IO ()
defaultMain = interactiveMain . const
