{-# LANGUAGE GADTs, RankNTypes, FlexibleContexts #-}
import Control.Monad.Cont
import Control.Monad.State
import Diagrams.Prelude
import Graphics.Rendering.Cairo hiding (identityMatrix, moveTo, translate)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.Pango
import Prelude hiding (lines)

import Diagrams.Backend.OpenGL.Internal
import Diagrams.Backend.OpenGL.Texture
import Diagrams.Backend.OpenGL.Models
import Diagrams.Backend.OpenGL.CmdLine

main :: IO ()
main = interactiveMain weird

weird :: Interaction
weird (Input x y t) = translate (fromIntegral x & fromIntegral y) $ mconcat [
    lw 10 $ fc yellow $ lc purple $ circle 200
    -- fc purple $ lc yellow $ square 400
    ]

boxes' :: Interaction
boxes' (Input x y t) = let mouseV = p2 (fromIntegral x, fromIntegral y) in
  lw 10 $ lc yellow $ position [
  --   (mouseV .+^ (r2 (100, 100)), square 100)
  -- , (mouseV .+^ (r2 (200, 200)), square 100)
  -- , (mouseV .+^ (r2 (300, 300)), square 100)
  --   (mouseV .+^ (r2 (0,   0)),   square 400)
  -- , (mouseV .+^ (r2 (0,   0)),   fromVertices [p2 (0, 0), p2 (400, 400)])
  ]

boxes :: Diagram OpenGL R2
boxes = lw 10 $ lc yellow $ position [
    (p2 (100, 100), square 100)
  , (p2 (200, 200), square 100)
  , (p2 (300, 300), square 100)
  , (p2 (0,   0),   square 400)
  , (p2 (0,   0),   fromVertices [p2 (0, 0), p2 (400, 400)])
  ]

smiley :: Diagram OpenGL R2
smiley = undefined

helloWorld :: Diagram OpenGL R2
helloWorld = moveTo (p2 (200, 200)) $ text "Hello World!"
