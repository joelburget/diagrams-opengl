{-# LANGUAGE GADTs, RankNTypes, FlexibleContexts #-}
import Control.Monad.Cont
import Control.Monad.State
import Data.Colour.SRGB
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
main = interactiveMain $ const helloWorld

weird :: Diagram OpenGL R2
weird  = mconcat [
    lw 10 $ fc (sRGB24read "#bbb") $ lc purple $ circle 200,
    fc purple $ lc yellow $ lw 5 $ square 400
    ]

boxes' :: Interaction
boxes' (Input x y t) = let mouseV = p2 (fromIntegral x, fromIntegral y) in
    moveTo mouseV (boxes <> helloWorld)

boxes :: Diagram OpenGL R2
boxes = lw 10 $ lc (sRGB24read "#bbbbbb") $ position [
    (p2 (100, 100), square 100)
  , (p2 (200, 200), square 100)
  , (p2 (300, 300), square 100)
  , (p2 (0,   0),   square 400)
  , (p2 (0,   0),   fromVertices [p2 (0, 0), p2 (400, 400)])
  ]

helloWorld :: Diagram OpenGL R2
helloWorld = lw 10 $ fc yellow $ text "Hello World!"
