module Main where

import Diagrams.Prelude
import Diagrams.Backend.OpenGL.Internal
import Diagrams.Backend.OpenGL.CmdLine

main :: IO ()
-- main = fst $ renderDia OpenGL (OpenGLOptions (Dims 800 800)) bezier
main = defaultMain bezier

bezier :: Diagram OpenGL R2
bezier = translate (r2 (300, -400)) $ illustrateBezier c1 c2 x2 where
    illustrateBezier c1 c2 x2
        =  endpt
        <> endpt  # translate x2
        <> ctrlpt # translate c1
        <> ctrlpt # translate c2
        <> l1
        <> l2
        <> fromSegments [bezier3 c1 c2 x2]
    dashed = dashing [10, 10] 0
    endpt  = circle 5 # fc red  # lw 0
    ctrlpt = circle 5 # fc blue # lw 0
    l1     = fromOffsets [c1] # dashed
    l2     = fromOffsets [x2 ^-^ c2] # translate c2 # dashed
    x2     = r2 (300, -100) :: R2
    [c1, c2] = map r2 [(100, 200), (300, 0)]
