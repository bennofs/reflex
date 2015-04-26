module Reflex.Brain.Cache
  ( cached
  , cachedIO
  , cachedIOWithInvalidate
  ) where

import Data.IORef
import Control.Monad
import Control.Monad.IO.Class
import System.IO.Unsafe
import Control.Applicative

cached :: MonadIO m => m a -> m a
cached = unsafePerformIO . cachedIO

cachedIO :: MonadIO m => m a -> IO (m a)
cachedIO = cachedIOWithInvalidate . const

cachedIOWithInvalidate :: MonadIO m => (IO () -> m a) -> IO (m a)
cachedIOWithInvalidate f = do
  ref <- newIORef (error "cachedIOWithInvalidate: uninitialized")
  let invalidate = writeIORef ref $ do
        r <- f invalidate
        liftIO $ writeIORef ref $ return r
        return r
  invalidate
  return . join . liftIO . readIORef $ ref
