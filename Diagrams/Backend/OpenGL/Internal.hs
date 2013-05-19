{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Diagrams.Backend.OpenGL.Internal where

import Control.Applicative ((<$>))
import Control.Monad (when)
import qualified Data.Foldable as F
import Data.Maybe
import Data.Typeable
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as CM
import qualified Graphics.Rendering.OpenGL as GL
import Diagrams.Prelude as P
import Diagrams.TwoD.Adjust
import Diagrams.TwoD.Text -- (Text)
import Diagrams.TwoD.Path

import qualified Diagrams.Backend.Cairo.Internal as DC
import Diagrams.Backend.OpenGL.Texture
import Diagrams.Backend.OpenGL.Models

import Data.Monoid.Split

data RetainedModel = RetainedModel
    { model    :: Model --GL.TextureObject
    , modified :: Bool
    } deriving Show

texModel :: Int -> Int -> Int -> Int -> GL.TextureObject -> IO RetainedModel
texModel x y w h tex = do
    q <- getQuadImageModel (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
    return $ RetainedModel (q {textures = [tex]}) True

cairoModel :: Int -> Int -> Int -> Int -> C.Render () -> IO RetainedModel
cairoModel x y w h r = withImageSurface C.FormatARGB32 w h $ \s -> do
    C.renderWith s r
    tex <- createTexture GL.Texture2D GL.Disabled $ texImage2DSurface Nothing 0 s
    texModel x y w h tex

data OpenGL = OpenGL
    deriving (Eq, Ord, Read, Show, Typeable)

instance Monoid (Render OpenGL R2) where
    mempty = O $ return ()
    (O rd1) `mappend` (O rd2) = O (rd1 >> rd2)

instance Backend OpenGL R2 where
    data Render  OpenGL R2 = O (C.Render ())
    type Result  OpenGL R2 = IO RetainedModel
    data Options OpenGL R2 = OpenGLOptions { openGLSizeSpec :: SizeSpec2D }

    withStyle _ s t (O render) = O $ do
        C.save
        cairoMiscStyle s
        render
        cairoTransf t
        cairoStrokeStyle s
        C.stroke
        C.restore

    -- doRender :: b -> Options b v -> Render b v -> Result b v
    doRender _ (OpenGLOptions size) (O render) = let
        (w, h) = case size of
            Width w'   -> (w', w')
            Height h'  -> (h', h')
            Dims w' h' -> (w', h')
            Absolute   -> (100, 100)
        in cairoModel 0 0 (round w) (round h) render

    -- TODO(joel)
    -- Options b v -> QDiagram b v m -> (Options b v, QDiagram b v m)
    -- adjustDia _ = (,)
    adjustDia _ o d = (o, setDefault2DAttributes d)

unO :: Render OpenGL R2 -> C.Render ()
unO (O x) = x

convert :: Options OpenGL R2 -> Diagram OpenGL R2 -> C.Render ()
convert opts d = sequence_ renders where
    renders :: [C.Render ()]
    renders = map (unO . renderOne) (prims d')
    d' :: Diagram OpenGL R2
    (opts', d') = adjustDia OpenGL opts d
    renderOne :: (Prim OpenGL R2, (Split (Transformation R2), Style R2)) ->
        Render OpenGL R2
    renderOne (p, (M t, s)) =
        withStyle OpenGL s mempty (render OpenGL (transform t p))
    renderOne (p, (t1 :| t2, s)) =
        withStyle OpenGL s t1 (render OpenGL (transform (t1 <> t2) p))

instance Renderable (Segment R2) OpenGL where
  render _ (Linear v) = O $ uncurry C.relLineTo (unr2 v)
  render _ (Cubic (unr2 -> (x1,y1))
                  (unr2 -> (x2,y2))
                  (unr2 -> (x3,y3)))
    = O $ C.relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail R2) OpenGL where
  render _ (Trail segs c) = O $ do
    mapM_ renderC segs
    when c C.closePath

instance Renderable (Path R2) OpenGL where
  render _ (Path trs) = O $ C.newPath >> F.mapM_ renderTrail trs
    where renderTrail (unp2 -> p, tr) = do
              uncurry C.moveTo p
              renderC tr

-- Use pango? Why did I not have this instance before?
instance Renderable Text OpenGL where
  render _ txt = O $ renderC txt

renderC :: (Renderable a OpenGL, V a ~ R2) => a -> C.Render ()
renderC a = case render OpenGL a of O r -> r

cairoMiscStyle :: Style v -> C.Render ()
cairoMiscStyle s =
  sequence_
  . catMaybes $ [ handle clip
                , handle fSize
                , handleFontFace
                , handle fColor
                , handle lFillRule
                ]
  where handle :: AttributeClass a => (a -> C.Render ()) -> Maybe (C.Render ())
        handle f = f `fmap` getAttr s
        clip     = mapM_ (\p -> renderC p >> C.clip) . getClip
        -- fSize    = C.setFontSize . getFontSize
        fSize    = \x -> C.setFontSize $ getFontSize x
        fFace    = fromMaybe "" $ getFont <$> getAttr s
        fSlant   = fromFontSlant  . fromMaybe FontSlantNormal
                 $ getFontSlant  <$> getAttr s
        fWeight  = fromFontWeight . fromMaybe FontWeightNormal
                 $ getFontWeight <$> getAttr s
        handleFontFace = Just $ C.selectFontFace fFace fSlant fWeight
        fColor c = setSource (getFillColor c) s
        -- lFillRule = C.setFillRule . fromFillRule . getFillRule
        lFillRule = \x -> C.setFillRule $ fromFillRule $ getFillRule x

fromFontSlant :: FontSlant -> C.FontSlant
fromFontSlant FontSlantNormal   = C.FontSlantNormal
fromFontSlant FontSlantItalic   = C.FontSlantItalic
fromFontSlant FontSlantOblique  = C.FontSlantOblique

fromFontWeight :: FontWeight -> C.FontWeight
fromFontWeight FontWeightNormal = C.FontWeightNormal
fromFontWeight FontWeightBold   = C.FontWeightBold

-- | Multiply the current transformation matrix by the given 2D
--   transformation.
cairoTransf :: T2 -> C.Render ()
cairoTransf t = C.transform m
  where m = CM.Matrix a1 a2 b1 b2 c1 c2
        (unr2 -> (a1,a2)) = apply t unitX
        (unr2 -> (b1,b2)) = apply t unitY
        (unr2 -> (c1,c2)) = transl t

-- | Handle style attributes having to do with stroke.
cairoStrokeStyle :: Style v -> C.Render ()
cairoStrokeStyle s = sequence_ . catMaybes $
  [ handle fColor
  , handle lColor  -- see Note [color order]
  , handle lWidth
  , handle lCap
  , handle lJoin
  , handle lDashing
  ]
  where handle :: (AttributeClass a) => (a -> C.Render ()) -> Maybe (C.Render ())
        handle f = f `fmap` getAttr s
        fColor c =  setSource (getFillColor c) s >> C.fillPreserve
        lColor c = setSource (getLineColor c) s
        lWidth   = C.setLineWidth . getLineWidth
        lCap     = C.setLineCap . fromLineCap . getLineCap
        lJoin    = C.setLineJoin . fromLineJoin . getLineJoin
        lDashing (getDashing -> Dashing ds offs) =
          C.setDash ds offs

-- | Set the source color.
setSource :: Color c => c -> Style v -> C.Render ()
setSource c s = C.setSourceRGBA r g b a'
  where (r,g,b,a) = colorToRGBA c
        a'        = case getOpacity <$> getAttr s of
                      Nothing -> a
                      Just d  -> a * d

{- ~~~~ Note [color order]

   It's important for the line and fill colors to be handled in the
   given order (fill color first, then line color) because of the way
   Cairo handles them (both are taken from the sourceRGBA).
-}

fromLineCap :: LineCap -> C.LineCap
fromLineCap LineCapButt   = C.LineCapButt
fromLineCap LineCapRound  = C.LineCapRound
fromLineCap LineCapSquare = C.LineCapSquare

fromLineJoin :: LineJoin -> C.LineJoin
fromLineJoin LineJoinMiter = C.LineJoinMiter
fromLineJoin LineJoinRound = C.LineJoinRound
fromLineJoin LineJoinBevel = C.LineJoinBevel

fromFillRule :: FillRule -> C.FillRule
fromFillRule Winding = C.FillRuleWinding
fromFillRule EvenOdd = C.FillRuleEvenOdd
