module Lib where

import Apecs
import Control.Monad
import Data.Function
import SDL hiding (initialize)
import qualified SDL
import Data.Semigroup

import ARPG.World

main :: IO ()
main = do
  (world, window, renderer) <- initialize
  runWith world $
    fix $ \loop -> do
      shouldQuit <- fmap or $ (liftIO pollEvents) >>= mapM (handleEvent . eventPayload)
      render renderer
      unless shouldQuit loop
  destroyWindow window

render :: SDL.Renderer -> System World ()
render renderer = do
  liftIO $ do
    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer
  liftIO $ rendererDrawColor renderer $= V4 0 255 0 255
  MousePosition (Option m) <- readGlobal
  case m of
    Just (Last p) -> 
      liftIO $ drawPoint renderer (fromIntegral <$> p)
    Nothing -> return ()
  liftIO $ do
    present renderer

handleEvent :: SDL.EventPayload -> System World Bool
handleEvent (MouseMotionEvent (MouseMotionEventData _ _ _ pos _)) = do
   writeGlobal $ MousePosition $ Option $ Just $ Last pos
   return False
handleEvent (WindowClosedEvent _) = return True
handleEvent (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeEscape _))) =
  return True
handleEvent _ = return False

initialize :: IO (World, Window, Renderer)
initialize = do
  world <- World <$> initStore <*> initStore
  (window, renderer) <- initializeSDL 
  return (world, window, renderer)

initializeSDL :: IO (Window, Renderer)
initializeSDL = do
  SDL.initialize [InitVideo]
  window <- createWindow "ARPG" defaultWindow
    { windowOpenGL = Just defaultOpenGL
    , windowInitialSize = V2 640 480 }
  cursorVisible $= False
  showWindow window
  renderer <- createRenderer window (-1) SDL.defaultRenderer
    { rendererType = UnacceleratedRenderer
    , rendererTargetTexture = True }
  return (window, renderer)

