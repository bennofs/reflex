{-# LANGUAGE RankNTypes #-}
module Reflex.Brain.Signal
 ( Signal()
 , signalEmit
 , signalConnect
 , signalPrune
 , signalReset
 , signalNever
 , signalCreate
 )
 where

import Data.Maybe
import System.Mem.Weak
import Control.Monad (filterM)
import Data.IORef
import Control.Monad.IO.Class
import Control.Applicative

import qualified Data.Foldable as F

data Signal f a = SignalNever | Signal
  { signalSlots :: IORef [(IO Bool, a -> f ())]
  }
      
signalNever :: Signal f a
signalNever = SignalNever

signalEmit :: MonadIO f => Signal f a -> a -> f ()
signalEmit SignalNever _ = return ()
signalEmit s@(Signal slots) v
  = liftIO (readIORef slots) >>= F.mapM_ (`snd` v)

signalConnect :: Signal f a -> (a -> f ()) -> IO (IO ())
signalConnect SignalNever _ = return (return ())
signalConnect s@(Signal slots) call = do
  isAlive <- newIORef True
  signalPrune s
  writeIORef isAlive False <$ modifyIORef slots ((readIORef isAlive, call) :)

signalPrune :: Signal f a -> IO ()
signalPrune SignalNever = return ()
signalPrune (Signal slots) = do
  s <- readIORef slots
  go s >>= writeIORef slots
 where
  go [] = return []
  go (x@(a,_):xs) = do
    b <- a
    if b
      then (x:) <$> go xs
      else go xs

signalReset :: Signal f a -> IO ()
signalReset SignalNever = return ()
signalReset (Signal slots) = writeIORef slots []

signalCreate :: IO (Signal f a)
signalCreate = Signal <$> newIORef []
