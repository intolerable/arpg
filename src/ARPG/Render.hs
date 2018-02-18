module ARPG.Render (render, setupRender) where

import Apecs
import Control.Monad
import Control.Monad.IO.Class
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL as GL
import SDL (GLContext, V2(..))
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import qualified Data.Array as Array
import qualified Data.Array.Storable as StorableArray

import ARPG.Logging
import ARPG.Shader
import ARPG.Types.World

triangleToRender :: Vector (Vertex3 GLfloat)
triangleToRender = Vector.fromList
  [ Vertex3 (-0.5) (-0.5) 0.0
  , Vertex3 0.5 (-0.5) 0.0
  , Vertex3 0.0 0.5 0.0
  ]

setupRender :: System World ()
setupRender =
  liftIO $ do
    blend $= Enabled
    multisample $= Enabled
    debugOutput $= Enabled
    vshader <- loadVertexShader
    fshader <- loadFragmentShader
    program <- createProgram
    attachShader program vshader
    attachShader program fshader
    linkProgram program
    lstat <- linkStatus program
    unless lstat $
      programInfoLog program >>= fail
    validateProgram program
    vstat <- validateStatus program
    unless vstat $
      programInfoLog program >>= fail
    currentProgram $= Just program
    vao <- genObjectName
    vbo <- genObjectName
    bindVertexArrayObject $= Just vao
    bindBuffer ArrayBuffer $= Just vbo
    vectorPtrWithLen triangleToRender $ \ptr len -> do
      print (ptr, len)
      bufferData ArrayBuffer $= (len, ptr, StaticDraw)
    vertexAttribPointer (AttribLocation 0) $=
      (ToNormalizedFloat, VertexArrayDescriptor 3 Float (sizeOfT @(Vertex3 GLfloat)) (wordPtrToPtr 0))
    vertexAttribArray (AttribLocation 0) $= Enabled
    -- return vao

temporarily :: MonadIO m => StateVar a -> a -> m b -> m b
temporarily s x act = do
  y <- GL.get s
  s $= x
  res <- act
  s $= y
  return res

render :: System World ()
render = do
  cullFace $= Nothing
  runGC
  -- st :: (MousePosition, ActiveAbility) <- readGlobal
  -- printLog st
  resizeViewport
  liftIO $ do
    clearColor $= Color4 0.25 0.75 1.0 0
    clear [ColorBuffer]
  setupRender
  liftIO $ do
    drawArrays Triangles 0 3

don't :: Applicative f => f a -> f ()
don't _ = pure ()

resizeViewport :: System World ()
resizeViewport = do
  WindowSize size <- readGlobal
  case size of
    Nothing -> return ()
    Just (V2 x y) -> do
      liftIO $ viewport $= (Position 0 0, Size x y)
      writeGlobal $ WindowSize Nothing

vectorPtrWithLen :: forall a i . (Storable a, Integral i) => Vector a -> (Ptr a -> i -> IO ()) -> IO ()
vectorPtrWithLen a f = do
  Vector.unsafeWith a $ \ptr -> f ptr (fromIntegral (Vector.length a) * sizeOfT @a)

sizeOfT :: forall a . Storable a => forall i . Integral i => i
sizeOfT = fromIntegral $ sizeOf (undefined :: a)
