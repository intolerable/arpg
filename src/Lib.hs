module Lib where

import Apecs hiding (Map)
import Control.Monad
import Data.Function
import Data.StateVar
import SDL
import Data.Semigroup
import qualified Graphics.Rendering.OpenGL as GL

import ARPG.Events
import ARPG.Logging
import ARPG.Render
import ARPG.Types.World

main :: IO ()
main = do
  (world, window) <- initializeARPG
  runWith world $ do
    setupRender
    fix $ \loop -> do
      EventResult (Any quitting) <- liftIO pollEvents >>= fmap mconcat .  traverse (handleEvent . eventPayload)
      render
      SDL.glSwapWindow window
      unless quitting loop
  destroyWindow window

initializeARPG :: IO (World, Window)
initializeARPG = do
  (,) <$> initializeWorld <*> initializeSDL

initializeSDL :: IO Window
initializeSDL = do
  SDL.initialize [InitVideo]
  window <- createWindow "gl" defaultWindow
    { windowOpenGL = Just defaultOpenGL
      { glProfile = Core Debug 3 3
      , glMultisampleSamples = 8 }
    , windowInitialSize = V2 640 480 }
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  SDL.glCreateContext window
  cursorVisible $= True
  GL.debugMessageCallback $= Just printLog
  showWindow window
  return window

