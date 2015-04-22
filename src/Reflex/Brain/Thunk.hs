module Reflex.Brain.Thunk
  ( Thunk()
  , thunk
  , eval
  ) where

import Data.IORef
import Control.Monad
import Control.Monad.IO.Class
import System.IO.Unsafe
import Control.Applicative

newtype Thunk m a = Thunk (m a)

instance Functor m => Functor (Thunk m) where
  fmap f (Thunk a) = Thunk $ f <$> a

instance Applicative m => Applicative (Thunk m) where
  pure = Thunk . pure
  Thunk f <*> Thunk a = Thunk $ f <*> a

instance Monad m => Monad (Thunk m) where
  return = Thunk . return
  Thunk a >>= f = Thunk $ a >>= (\(Thunk x) -> x) . f

thunk :: MonadIO m => m a -> Thunk m a
thunk m = unsafePerformIO $ do
  ref <- newIORef (error "unintialized")
  writeIORef ref $ m >>= \x -> liftIO . liftM (const x) . writeIORef ref $ return x
  return $ Thunk $ join $ liftIO $ readIORef ref

eval :: (Monad m, MonadIO m) => Thunk m a -> m a
eval (Thunk a) = a
