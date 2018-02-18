module ARPG.Shader where

import Control.Monad
import Graphics.Rendering.OpenGL
import qualified Data.ByteString.Char8 as BS

import Paths_arpg

loadVertexShader :: IO Shader
loadVertexShader = do
  shaderSrc <- getDataFileName "shader/vertex.glsl" >>= BS.readFile
  shader <- createShader VertexShader
  shaderSourceBS shader $= shaderSrc
  compileShader shader
  success <- compileStatus shader
  unless success $ shaderInfoLog shader >>= fail
  return shader

loadFragmentShader :: IO Shader
loadFragmentShader = do
  shaderSrc <- getDataFileName "shader/fragment.glsl" >>= BS.readFile
  shader <- createShader FragmentShader
  shaderSourceBS shader $= shaderSrc
  compileShader shader
  success <- compileStatus shader
  unless success $ shaderInfoLog shader >>= fail
  return shader
