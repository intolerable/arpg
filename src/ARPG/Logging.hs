module ARPG.Logging where

import Control.Monad.IO.Class
import Foreign.C.String
import qualified SDL.Raw.Basic

putStrLog :: MonadIO m => String -> m ()
putStrLog str = liftIO $
  withCString str SDL.Raw.Basic.log

printLog :: (Show a, MonadIO m) => a -> m ()
printLog = putStrLog . show
