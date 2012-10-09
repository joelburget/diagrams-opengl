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
-- import Diagrams.TwoD.Image (Image)
import Diagrams.TwoD.Text (Text)
import Graphics.Rendering.Diagrams.Monoids

-- import Diagrams.Backend.OpenGL.Matrix
import Diagrams.Backend.OpenGL.Texture
import Diagrams.Backend.OpenGL.Models

import Debug.Trace

data RetainedModel = RetainedModel
    { model    :: Model --GL.TextureObject
    , modified :: Bool
    } deriving Show

data OpenGL = OpenGL
    deriving (Eq, Ord, Read, Show, Typeable)

type ModelKey = Unique.Id

type OpenGLRenderM a = StateT (Map.Map ModelKey RetainedModel) C.Render a

runOpenGLRender :: OpenGLRenderM a -> C.Render (a, Map.Map ModelKey RetainedModel)
runOpenGLRender rndr = runStateT rndr (fromList [])

evalOpenGLRender :: OpenGLRenderM a -> C.Render a
evalOpenGLRender = liftM fst . runOpenGLRender

execOpenGLRender :: OpenGLRenderM a -> C.Render (Map.Map ModelKey RetainedModel)
execOpenGLRender = liftM snd . runOpenGLRender

runRender :: C.Surface -> OpenGLRenderM a -> IO a
runRender surface = C.renderWith surface . evalOpenGLRender

save :: OpenGLRenderM ()
save = lift C.save

restore :: OpenGLRenderM ()
restore = lift C.restore

getSurfaces :: OpenGLRenderM [RetainedModel]
getSurfaces = fmap elems get

drawSurfaces :: [RetainedModel] -> IO ()
drawSurfaces models = forM_ models $ \m ->
    -- when (modified m) $ drawModel $ trace (show (model m)) model m
    drawModel $ trace ("drawSurfaces: " ++ (show (model m))) model m

instance Monoid (Render OpenGL R2) where
    mempty = O $ return ()
    (O rd1) `mappend` (O rd2) = O (rd1 >> rd2)

unO :: Render OpenGL R2 -> OpenGLRenderM ()
unO (O x) = x

splitAndRender :: Monoid' m => QDiagram OpenGL R2 m -> OpenGLRenderM ()
splitAndRender dia  = forM_ ps (unO . renderOne) where
    ps :: [(Prim OpenGL R2, (Split (Transformation R2), Style R2))]
    ps = prims dia
    renderOne :: (Prim OpenGL R2, (Split (Transformation R2), Style R2)) ->
        Render OpenGL R2
    renderOne (p, (M t, s)) =
        withStyle OpenGL s mempty (render OpenGL (transform t p))
    renderOne (p, (t1 :| t2, s)) =
        withStyle OpenGL s t1 (render OpenGL (transform (t1 <> t2) p))

instance Backend OpenGL R2 where
    data Render  OpenGL R2 = O (OpenGLRenderM ())
    type Result  OpenGL R2 = OpenGLRenderM ()
    data Options OpenGL R2 = OpenGLOptions { openGLSizeSpec :: SizeSpec2D }

    withStyle _ _ _ (O render) = O render

-- TODO use `prims :: QDiagram b v m -> [(Prim b v, (Split (Transformation v), Style v))]` to split a diagram into retained models. The default implementation of renderDia uses prims also.

    -- doRender :: OpenGL -> OpenGLOptions -> O (OpenGLRenderM ()) -> (IO (), OpenGLRenderM ())
    doRender _ (OpenGLOptions size) (O render) = (renderIO, render) where
        renderIO = do
            let models :: C.Render [RetainedModel]
                -- models = evalStateT (render >> getSurfaces) (fromList [])
                models = evalStateT getSurfaces (fromList [])
                (w, h) = case size of
                    Width w'   -> (w', w')
                    Height h'  -> (h', h')
                    Dims w' h' -> (w', h')
                    Absolute   -> (100, 100)
            withImageSurface C.FormatARGB32 (round w) (round h) $ \surface -> do
                putStrLn "withImageSurface"
                models' <- C.renderWith surface models :: IO [RetainedModel]
                putStrLn "drawSurfaces"
                drawSurfaces models'
                trace (show models') $ return ()

    -- TODO(joel)
    adjustDia _ = (,)

    -- * split into primitives
    -- * decide what's changed
    -- * render each piece
    renderDia b opts d = let
        (OpenGLOptions size, d') = adjustDia b opts d
        (w, h) = case size of
            Width w'   -> (w', w')
            Height h'  -> (h', h')
            Dims w' h' -> (w', h')
            Absolute   -> (100, 100)

        rndr :: OpenGLRenderM ()
        rndr = splitAndRender d'

        ioRndr = withImageSurface C.FormatARGB32 (round w) (round h) $ \surface -> do
            putStrLn "withImageSurface'"
            runRender surface rndr

        in (ioRndr, rndr)

instance Renderable (Segment R2) OpenGL where
  render _ (Linear v) = trace "render linear segment" $ O . lift $ uncurry C.relLineTo (unr2 v)
  render _ (Cubic (unr2 -> (x1,y1))
                  (unr2 -> (x2,y2))
                  (unr2 -> (x3,y3)))
    = trace "render cubic segment" $ O . lift $ C.relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail R2) OpenGL where
  render _ (Trail segs c) = O $ do
    trace "render trail: (" $ return ()
    mapM_ renderC segs
    trace ")" $ return ()
    lift $ when c C.closePath

instance Renderable (Path R2) OpenGL where
  -- render _ (Path trs) = trace "render path" $ O $ lift C.newPath >> F.mapM_ renderTrail trs
  render _ (Path trs) = O $ do
    trace "render path: (" $ return ()
    lift C.newPath
    F.mapM_ renderTrail trs
    trace ")" $ return ()
    where renderTrail (unp2 -> p, tr) = do
            lift $ uncurry C.moveTo p
            renderC tr

-- Use pango? Why did I not have this instance before?
instance Renderable Text OpenGL where
  render _ txt =  trace "render text" $ O $ renderC txt

renderC :: (Renderable a OpenGL, V a ~ R2) => a -> OpenGLRenderM ()
renderC a = case render OpenGL a of O r -> r
