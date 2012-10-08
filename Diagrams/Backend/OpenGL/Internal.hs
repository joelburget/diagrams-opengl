{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Diagrams.Backend.OpenGL.Internal where

import Control.Monad.State
import Data.Typeable
import qualified Graphics.Rendering.Cairo as C
import qualified Data.Foldable as F
import Data.Unique.Id as Unique
import Data.Map as Map

import Diagrams.Prelude as P
import Diagrams.TwoD.Image (Image)

import Diagrams.Backend.OpenGL.Matrix
import Diagrams.Backend.OpenGL.Texture
import Diagrams.Backend.OpenGL.Models

data RetainedModel = RetainedModel
    { model    :: Model --GL.TextureObject
    , modified :: Bool
    }

data OpenGL = OpenGL
    deriving (Eq, Ord, Read, Show, Typeable)

type ModelKey = Unique.Id

type OpenGLRenderM a = StateT (Map.Map ModelKey RetainedModel) C.Render a

save :: OpenGLRenderM ()
save = lift C.save

restore :: OpenGLRenderM ()
restore = lift C.restore

getSurfaces :: OpenGLRenderM [RetainedModel]
-- getSurfaces = elems . get
getSurfaces = get >>= return . elems

drawSurfaces :: [RetainedModel] -> IO ()
drawSurfaces models = forM_ models $ \m ->
    when (modified m) $ drawModel $ model m

instance Monoid (Render OpenGL R2) where
    mempty = O $ return ()
    (O rd1) `mappend` (O rd2) = O (rd1 >> rd2)

instance Backend OpenGL R2 where
    data Render  OpenGL R2 = O (OpenGLRenderM ())
    type Result  OpenGL R2 = (IO (), OpenGLRenderM ())
    data Options OpenGL R2 = OpenGLOptions
        { openGLSizeSpec :: SizeSpec2D }

    withStyle _ style trans (O render) = O render

    -- doRender :: OpenGL -> OpenGLOptions -> O (OpenGLRenderM ()) -> (IO (), OpenGLRenderM ())
    doRender _ (OpenGLOptions size) (O render) = (renderIO, render) where
        renderIO = do
            let models = execStateT (render >> getSurfaces) []
            drawSurfaces models
            return ()

    -- TODO(joel)
    -- adjustDia

instance Renderable (Segment R2) OpenGL where
  render _ (Linear v) = O . lift $ uncurry C.relLineTo (unr2 v)
  render _ (Cubic (unr2 -> (x1,y1))
                  (unr2 -> (x2,y2))
                  (unr2 -> (x3,y3)))
    = O . lift $ C.relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail R2) OpenGL where
  render _ (Trail segs c) = O $ do
    mapM_ renderC segs
    lift $ when c C.closePath

instance Renderable (Path R2) OpenGL where
  render _ (Path trs) = O $ lift C.newPath >> F.mapM_ renderTrail trs
    where renderTrail (unp2 -> p, tr) = do
            lift $ uncurry C.moveTo p
            renderC tr

renderC :: (Renderable a OpenGL, V a ~ R2) => a -> OpenGLRenderM ()
renderC a = case (render OpenGL a) of O r -> r
