{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Reflex.Brain where

import Data.Dependent.Sum
import Control.Monad.Primitive
import System.Mem.Weak
import Reflex.Brain.Thunk
import Reflex.Class
import Reflex.Brain.Deps
import Reflex.Brain.Signal
import Reflex.Host.Class
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.IORef
import Control.Applicative
import Data.Maybe
import System.IO.Unsafe

import qualified Data.Foldable as F
import qualified Data.Dependent.Map as DM
import qualified Data.Traversable as T

data Brain

data Subscribable a = Subscribable
  { subscribe :: Thunk IO (Deps, a)
  }

data EventSubscribed a = EventSubscribed
  { eventOccurrence :: IO (Maybe a)
  , eventSignal :: Signal (PushM Brain) a
  }

newtype BrainHandle a = BrainHandle { unBrainHandle :: WithDeps (EventSubscribed a) }
data BrainTrigger a = BrainTrigger
  { triggerSignal :: Signal (PushM Brain) a
  , triggerSetup :: a -> IO ()
  }
  
instance Reflex Brain where
  newtype Event Brain a = Event (Subscribable (EventSubscribed a))
  newtype Behavior Brain a = Behavior ()
  type PullM Brain = IO
  type PushM Brain = IO

  push f (Event s) = Event . Subscribable . thunk $ do
    (evDeps, ev) <- eval (subscribe s)
    occRef <- eventOccurrence ev >>= T.traverse f >>= newIORef . join
    signal <- signalCreate
    signalConnect (eventSignal ev) <=< slotForIORef occRef $ \write a -> do
      mOcc <- f a
      F.forM_ mOcc $ \occ -> do
        liftIO $ write (Just occ)
        signalTrigger signal occ
    pure (evDeps `addDep` occRef, EventSubscribed (readIORef occRef) signal)

instance MonadSample Brain IO where

instance MonadHold Brain IO where

newtype Head a = Head { runHead :: IO a } deriving (Functor, Applicative, Monad, MonadIO)

instance ReflexHost Brain where
  type EventTrigger Brain = BrainTrigger
  type EventHandle  Brain = BrainHandle
  type HostFrame    Brain = IO

instance MonadReflexCreateTrigger Brain Head where
  newEventWithTrigger f = Head $ do
    occRef <- newIORef Nothing
    occWeakRef <- mkWeakIORef occRef $ pure ()
    signal <- signalCreate
    register <- newIORef (error "uninitialized")
    writeIORef register $ do
      deinit <- f $ BrainTrigger signal $ \v -> 
        deRefWeak occWeakRef >>= F.mapM_ (`writeIORef` Just v)
      void $ mkWeakIORef occRef deinit
      writeIORef register $ return ()
    pure . Event . Subscribable . thunk $ do
      join $ readIORef register
      pure (dep occRef, EventSubscribed
        { eventOccurrence = readIORef occRef
        , eventSignal = signal
        })

newtype HeadRead a = HeadRead { runRead :: IO a } deriving (Functor, Applicative, Monad)
instance MonadReadEvent Brain HeadRead where
  readEvent (BrainHandle h) = HeadRead $ fmap return <$> useWithDeps h eventOccurrence

instance MonadReflexHost Brain Head where
  fireEventsAndRead evs r = Head $ do
    F.forM_ evs $ \(t :=> v) -> triggerSetup t v
    F.forM_ evs $ \(t :=> v) -> signalTrigger (triggerSignal t) v
    runRead r
  subscribeEvent (Event ev) = Head . fmap BrainHandle $
    eval (subscribe ev) >>= uncurry withDeps
