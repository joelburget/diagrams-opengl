{-# LANGUAGE GADTs, RankNTypes, FlexibleContexts #-}
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Task
import Diagrams.Prelude
import Graphics.Rendering.Cairo hiding (identityMatrix, moveTo)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.Pango
import Graphics.UI.GLFW.Task
import qualified Graphics.UI.GLFW as GLFW
import Prelude hiding (lines)

import Diagrams.Backend.OpenGL.Internal
import Diagrams.Backend.OpenGL.Texture
import Diagrams.Backend.OpenGL.Models
import Diagrams.Backend.OpenGL.CmdLine

-- getLines :: M Lines
-- getLines = fmap lines get
-- 
-- modifyLines :: MonadState World m => (Lines -> Lines) -> m ()
-- modifyLines f = modify $ \x -> x { lines = f (lines x), dirty = True }

-- getSurfaces :: M [RetainedModel]
-- getSurfaces = fmap models get

main :: IO ()
main = defaultMain $ boxes -- <> helloWorld

{-
w, h :: Int
w = 400
h = 400
-}

-- drawSurfaces :: [RetainedModel] -> IO ()
-- drawSurfaces models = forM_ models $ \m ->
--     when (modified m) $ drawModel $ model m

{-
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
-}

boxes :: Diagram OpenGL R2
boxes = lw 10 $ lc yellow $ position [
    (p2 (100, 100), square 100)
  , (p2 (200, 200), square 100)
  , (p2 (300, 300), square 100)
  , (p2 (0,   0),   square 400)
  , (p2 (0,   0),   fromVertices [p2 (0, 0), p2 (400, 400)])
  ]

{-
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
-}
smiley :: Diagram OpenGL R2
smiley = undefined

{-
helloWorld :: IO RetainedModel
helloWorld = textModel 200 200 AlignCenter "Hello World!"
-}

helloWorld :: Diagram OpenGL R2
helloWorld = moveTo (p2 (200, 200)) $ text "Hello World!"
