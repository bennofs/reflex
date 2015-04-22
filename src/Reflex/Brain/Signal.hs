module Reflex.Brain.Signal where

import Data.Maybe
import System.Mem.Weak
import Control.Monad (filterM)
import Data.IORef
import Control.Monad.IO.Class
import Control.Applicative

import qualified Data.Foldable as F

data Slot f a = Slot
  { slotCall :: a -> f ()
  , slotDead :: IO Bool
  }

slotForever :: (a -> f ()) -> Slot f a
slotForever f = Slot f (return False)

slotForIORef :: MonadIO f => IORef b -> ((b -> IO ()) -> a -> f ()) -> IO (Slot f a)
slotForIORef ref f = do
  weakRef <- mkWeakIORef ref (return ())
  let handler v = do
        ref' <- liftIO $ deRefWeak weakRef
        maybe (return ()) (flip f v . writeIORef) ref'
  return $ Slot handler $ isNothing <$> deRefWeak weakRef

data Signal f a = Signal
  { signalTrigger :: a -> f ()
  , signalConnect :: Slot f a -> IO ()
  , signalPrune   :: IO ()
  }

signalNever :: Applicative f => Signal f a
signalNever = Signal (\_ -> pure ()) (\_ -> pure ()) (pure ())

signalCreate :: MonadIO f => IO (Signal f a)
signalCreate = do
  slots <- newIORef []
  pure $ Signal
    { signalTrigger = \v -> liftIO (readIORef slots) >>= F.mapM_ (flip slotCall v)
    , signalConnect = modifyIORef slots . (:)
    , signalPrune   = readIORef slots >>= filterM (fmap not . slotDead) >>= writeIORef slots
    }
