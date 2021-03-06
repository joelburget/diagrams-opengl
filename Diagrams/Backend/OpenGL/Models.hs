{- |
    Helpers for creating and drawing bundled models.
-}
module Diagrams.Backend.OpenGL.Models (
    Vert3,
    Tex2,
    Width,
    Height,
    Model (..),
    fillScreen,
    drawModel,
    drawInstance,
    -- createImageModel,
    -- createTextModel,
    createModel,
    getHexModel,
    getQuadModel,
    getTriModel,
    getQuadImageModel,
    hexVerts,
    hexTexCoords,
    quadVerts,
    quadTexCoords,
    triVerts,
    triTexCoords,
    quadImageVerts,
    quadImageTexCoords
) where

import Graphics.Rendering.OpenGL hiding (Height)
import Diagrams.Backend.OpenGL.VBO
import Diagrams.Backend.OpenGL.Texture
import Diagrams.Backend.OpenGL.Matrix
import Data.IORef
import System.IO.Unsafe
import Foreign.Ptr

-- | 3D Vertex
type Vert3 = (GLfloat, GLfloat, GLfloat)

-- | 2D Texture coordinate
type Tex2 = (GLfloat, GLfloat)

type Width = Int
type Height = Int

-- | Model that bundles a VBO and textures together into a drawable object.
data Model = Model
    { mode      :: PrimitiveMode
    -- ^Mode of the vertices, e.g. TriangleFan
    , count     :: NumArrayIndices
    -- ^Vertex count
    , vbo       :: BufferObject
    -- ^VBO with the vertices and texcoords
    , textures  :: [TextureObject]
    -- ^List of textures to bind with 'Texture.useTexture'
    , verts     :: VertexArrayDescriptor GLfloat
    -- ^Vertex array descriptor for the model vertices
    , texCoords :: VertexArrayDescriptor GLfloat
    -- ^Vertex array descriptor for the model texture coordinates
    } deriving Show

-- | Fills screen with the current color.
--
--   Loads the identity matrix and disables Texture2D.
fillScreen :: IO ()
fillScreen = do
    glLoadMatrix identityMatrix
    texture Texture2D $= Disabled
    renderPrimitive TriangleFan $ mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) quadVerts

-- | Draws a 'Model' to the screen.
drawModel :: Model -> IO ()
drawModel model = do
    bindBuffer ArrayBuffer $= Just (vbo model)
    clientState VertexArray $= Enabled
    arrayPointer VertexArray $= verts model
    clientState TextureCoordArray $= Enabled
    arrayPointer TextureCoordArray $= texCoords model
    useTexture (textures model)
    drawInstance model

-- | Calls drawArrays on the 'Model' values.
--   Useful for drawing the same model several times without state setting
--   overhead.
drawInstance :: Model -> IO ()
drawInstance model = drawArrays (mode model) 0 (count model)

-- | Creates a model from a list of vertices and texture coords.
createModel :: PrimitiveMode -> [Vert3] -> [Tex2] -> IO Model
createModel vertType vertList texCoordList = do
    vbo' <- createVBO elems
    return Model { mode=vertType
                 , count=fromIntegral vertCount
                 , vbo=vbo'
                 , textures=[]
                 , verts=vertsDesc
                 , texCoords=texCoordsDesc}
    where vertCount = min (length vertList) (length texCoordList)
          elems = foldl1 (++) $ zipWith con vertList texCoordList
          vboStride = 5*4
          vertsDesc = VertexArrayDescriptor 3 Float vboStride $ offset 0
          texCoordsDesc = VertexArrayDescriptor 2 Float vboStride $ offset (3*4)
          con (x,y,z) (s,t) = [x,y,z,s,t]
          offset = plusPtr nullPtr

-- | Vertices for a TriangleFan hexagon model centered on the origin with a radius of 1.
hexVerts :: [(GLfloat,GLfloat,GLfloat)]
hexVerts = map (\k -> (sin(2*pi*k/6), cos(2*pi*k/6), 0.0)) [1..6]

-- | Texture coords for the hexagon model.
hexTexCoords :: [(GLfloat, GLfloat)]
hexTexCoords = map (\(x,y,_) -> ((x+1.0)*0.5, 1.0-(y+1.0)*0.5)) hexVerts

-- | Gets a cached copy of the hexagon model.
getHexModel :: IO Model
getHexModel =
    getCachedModel hexModel (createModel TriangleFan hexVerts hexTexCoords)

-- | Vertices for a TriangleFan quad model centered on the origin with a radius of 1.
quadVerts :: [(GLfloat, GLfloat, GLfloat)]
quadVerts = [(-1,-1,0), (-1,1,0), (1,1,0), (1,-1,0)]

-- | Texture coords for the quad model.
quadTexCoords :: [(GLfloat, GLfloat)]
quadTexCoords = [(0,0), (0,1), (1,1), (1,0)]

-- | Gets a cached copy of the quad model.
getQuadModel :: IO Model
getQuadModel =
    getCachedModel quadModel (createModel TriangleFan quadVerts quadTexCoords)

-- | Vertices for a triangle model centered on the origin with a radius of 1.
triVerts :: [(GLfloat, GLfloat, GLfloat)]
triVerts = [(-1,-1,0), (0,1,0), (1,-1,0)]

-- | Texture coords for the triangle model.
triTexCoords :: [(GLfloat, GLfloat)]
triTexCoords = [(0,0), (0.5,1), (1,0)]

-- | Gets a cached copy of the triangle model.
getTriModel :: IO Model
getTriModel =
    getCachedModel triModel (createModel Triangles triVerts triTexCoords)

-- | Vertices for a TriangleFan quad image model with bottom-left corner on
--   the origin and side length of 1.
quadImageVerts :: [(GLfloat, GLfloat, GLfloat)]
quadImageVerts = [(0,0,0), (0,1,0), (1,1,0), (1,0,0)]

-- | Texture coords for the quad image model, flipped vertically so that y-down
--   images display correctly.
quadImageTexCoords :: [(GLfloat, GLfloat)]
--quadImageTexCoords = [(0,1), (0,0), (1,0), (1,1)]
quadImageTexCoords = [(0, 0), (0, 1), (1, 1), (1, 0)]

-- | Gets a cached copy of the quad image model.
getQuadImageModel :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO Model
getQuadImageModel x' y' w h =
    let verts' = map (\(x, y, z) -> (x * w + x', y * h + y', z)) quadImageVerts
    --in getCachedModel imageModel (createModel TriangleFan verts' quadImageTexCoords)
    in do
        ref <- newIORef Nothing
        iq <- createModel TriangleFan verts' quadImageTexCoords
        ref $= Just iq
        return iq

-- -- | Loads an image file as an image model.
-- createImageModel :: FilePath -> IO (Width, Height, Model)
-- createImageModel fn = do
--     (w,h,tex) <- loadTexture fn
--     q <- getQuadImageModel
--     return (w,h,q {textures = [tex]})

-- -- | Creates a text image model from a 'LayoutAlignment' and 'Markup'.
-- createTextModel :: LayoutAlignment -> Markup -> IO (Width, Height, Model)
-- createTextModel alignment markup = do
--     (w,h,tex) <- createTextTexture alignment markup
--     q <- getQuadImageModel
--     return (w,h,q {textures = [tex]})


-- model caching

imageModel :: IORef (Maybe Model)
imageModel = unsafePerformIO (newIORef Nothing)
{-# NOINLINE imageModel #-}
hexModel :: IORef (Maybe Model)
hexModel = unsafePerformIO (newIORef Nothing)
{-# NOINLINE hexModel #-}
triModel :: IORef (Maybe Model)
triModel = unsafePerformIO (newIORef Nothing)
{-# NOINLINE triModel #-}
quadModel :: IORef (Maybe Model)
quadModel = unsafePerformIO (newIORef Nothing)
{-# NOINLINE quadModel #-}

getCachedModel :: IORef (Maybe Model) -> IO Model -> IO Model
getCachedModel ioref create = do
    im <- get ioref
    case im of
        Just q -> return q
        Nothing -> do
            iq <- create
            ioref $= Just iq
            return iq
