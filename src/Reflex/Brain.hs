{-# LANGUAGE FlexibleContexts #-}
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

import Control.Monad.Ref
import Data.Functor.Misc
import Data.Dependent.Sum
import Reflex.Brain.Cache
import Control.Monad.Trans.Maybe
import Reflex.Class
import Reflex.Brain.Deps
import Reflex.Brain.Signal
import Reflex.Host.Class
import Control.Monad
import Control.Monad.State
import Data.IORef
import Control.Applicative
import Data.Maybe

import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import qualified Data.Dependent.Map as DM
import qualified Data.Traversable as T

data Brain

data PropagateState = PropagateState
  { propagateFinish :: !(IO ())
  , propagateInvalidate :: !(IO ())
  , propagateDelayed :: !(IM.IntMap (PropagateM ()))
  }

newtype PropagateM a = PropagateM { unPropagateM :: StateT PropagateState IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runPropagateHead :: PropagateM a -> Head a
runPropagateHead (PropagateM a) = Head $
  evalStateT a $ PropagateState (pure ()) (pure ()) IM.empty

popDelayed :: Monad m => StateT PropagateState m (Maybe (Int, PropagateM ()))
popDelayed = do
  n <- gets $ IM.minViewWithKey . propagateDelayed
  T.forM n $ \((k,v), m') -> do
    modify' (\s -> s { propagateDelayed = m' })
    return (k,v)

runPropagate :: PropagateM a -> IO a
runPropagate (PropagateM m) = do
  (a, s) <- flip runStateT (PropagateState (pure ()) (pure ()) IM.empty) $
    m <* (runMaybeT . forever $ MaybeT popDelayed >>= lift . unPropagateM . snd)
  propagateFinish s
  a <$ propagateInvalidate s

modify' :: MonadState a m => (a -> a) -> m ()
modify' f = do
  s <- get
  put $! f s

scheduleFinish :: IO () -> PropagateM ()
scheduleFinish o = PropagateM $ modify' $ \s -> s
  { propagateFinish = propagateFinish s >> o
  }

scheduleInvalidate :: IO () -> PropagateM ()
scheduleInvalidate o = PropagateM $ modify' $ \s -> s
  { propagateInvalidate = propagateInvalidate s >> o
  }

scheduleDelayed :: Int -> PropagateM () -> PropagateM ()
scheduleDelayed height action = PropagateM $ modify' $ \s -> s
  { propagateDelayed = IM.insert height action (propagateDelayed s)
  }

data Push a = Push
  { pushOccurrence :: !(IO (Maybe a))
  , pushFired :: !(Signal PropagateM a)
  , pushHeight :: !(IO Int)
  , pushHeightInvalidated :: !(Signal PropagateM ())
  }

invalidHeight :: Int
invalidHeight = -1

data Pull a = Pull
  { pullValue :: !(IO a)
  , pullInvalidated :: !(Signal IO ())
  }

data TrackedState = TrackedState
  { trackedInvalidators :: ![Signal IO ()]
  }

newtype TrackedM a = TrackedM (StateT TrackedState (ManagedT IO) a)
  deriving (Functor, Applicative, Monad)

instance MonadSample Brain TrackedM where
  sample (Behavior b) = TrackedM $ do
    p <- liftIO b >>= lift . managedDependent
    modify' $ \s -> s
      { trackedInvalidators = pullInvalidated p : trackedInvalidators s
      }
    liftIO $ pullValue p
  
instance Reflex Brain where
  newtype Event Brain a = Event { eventPush :: PropagateM (Dependent (Push a)) }
  newtype Behavior Brain a = Behavior { behaviorPull :: IO (Dependent (Pull a)) }
  type PullM Brain = TrackedM
  type PushM Brain = EventM

  constant c = Behavior $ pure.pure $ Pull
    { pullValue = return c
    , pullInvalidated = signalNever
    }

  never = Event . pure . pure $ Push
    { pushOccurrence = return Nothing
    , pushFired = signalNever
    , pushHeight = return 0
    , pushHeightInvalidated = signalNever
    }
    
  push f event = Event . cached . runManagedT Nothing $ \occRef -> do
    p <- lift (eventPush event) >>= managedDependent
    signal <- liftIO signalCreate
    let handler a = do
          mOcc <- runEventM $ f a
          F.forM_ mOcc $ \occ -> do
            liftIO $ writeIORef occRef (Just occ)
            scheduleFinish $ writeIORef occRef Nothing
            signalEmit signal occ
    managedConnect (pushFired p) handler
    liftIO (pushOccurrence p) >>= F.traverse_ (lift . handler)
    pure $ Push
      { pushOccurrence = readIORef occRef
      , pushFired = signal
      , pushHeight = pushHeight p
      , pushHeightInvalidated = pushHeightInvalidated p
      }

  pull (TrackedM m) = Behavior . cached $
    runManagedT (error "pull: uninitialized valueRef") $ \valueRef -> do
      invalidated <- liftIO signalCreate
      cachedValue <- liftIO . cachedIOWithInvalidate $ \invalidate ->
       runManagedT () $ \_ -> do
        (val, s) <- runStateT m $ TrackedState []
        liftIO $ signalReset invalidated
        forM_ (trackedInvalidators s) $ \sig ->
          managedConnect sig $ \() -> do
            invalidate
            signalEmit invalidated ()
        pure val
      readValue <- managedDynDependent cachedValue
      pure Pull
        { pullValue = readValue
        , pullInvalidated = invalidated
        }

  merge eventMap = Event . cached . runManagedT DM.empty $ \accumRef -> do
    let toPush :: DSum (WrapArg (Event Brain) k)
               -> ManagedT PropagateM (DSum (WrapArg Push k))
        toPush (WrapArg x :=> event) = do
          p <- lift (eventPush event) >>= managedDependent
          return $ WrapArg x :=> p
    pushs <- mapM toPush $ DM.toList eventMap    
    fired <- liftIO signalCreate
    heightRef <- liftIO $ newIORef invalidHeight
    heightInvalidated <- liftIO signalCreate

    let
      toHeight :: MonadIO m => DSum (WrapArg Push k) -> m Int
      toHeight (WrapArg _ :=> p) = liftIO $ pushHeight p

      forMUnit_ :: Monad m => [a] -> (a -> m ()) -> m ()
      forMUnit_ = forM_

      getHeight = do
        height <- readIORef heightRef
        if height == invalidHeight
          then do
            height' <- maximum <$> mapM toHeight pushs
            height' <$ writeIORef heightRef height'
          else return height
      scheduledAction scheduledHeight = do
        height <- liftIO getHeight
        case compare height scheduledHeight of
          LT -> error "A merge's height has decreased after it has been scheduled."
          EQ -> signalEmit fired =<< liftIO (readIORef accumRef)
          GT -> scheduleDelayed height $ scheduledAction height

    forMUnit_ pushs $ \(WrapArg _ :=> p) ->
      void $ managedConnect (pushHeightInvalidated p) $ \_ -> do
        height <- liftIO $ readIORef heightRef
        unless (height == invalidHeight) $ do
          liftIO $ writeIORef heightRef invalidHeight
          signalEmit heightInvalidated ()

    forMUnit_ pushs $ \(WrapArg key :=> p) -> do
      let handler v = do
            accum <- liftIO $ readIORef accumRef
            when (DM.null accum) $ do
              height <- liftIO getHeight
              scheduleDelayed height $ scheduledAction height
              scheduleFinish $ writeIORef accumRef DM.empty
            liftIO $ writeIORef accumRef $! DM.insert key v accum
      void $ managedConnect (pushFired p) handler
      liftIO (pushOccurrence p) >>= lift . F.mapM_ handler
    
    pure Push
      { pushOccurrence = do
          v <- readIORef accumRef
          return $ if DM.null v then Nothing else Just v
      , pushFired = fired
      , pushHeight = getHeight         
      , pushHeightInvalidated = heightInvalidated
      }

newtype Head a = Head { runHead :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

instance MonadRef Head where
  type Ref Head = IORef
  newRef = liftIO . newIORef
  readRef = liftIO . readIORef
  writeRef r = liftIO . writeIORef r

instance MonadAtomicRef Head where
  atomicModifyRef f r = liftIO $ atomicModifyRef f r 

newtype BrainHandle a = BrainHandle { unBrainHandle :: Dependent (Push a) }

data BrainTrigger a = BrainTrigger
  { triggerSignal :: Signal PropagateM a
  , triggerSetup :: a -> IO ()
  }

instance ReflexHost Brain where
  type EventTrigger Brain = BrainTrigger
  type EventHandle  Brain = BrainHandle
  type HostFrame    Brain = IO

instance MonadReflexCreateTrigger Brain Head where
  newEventWithTrigger f = Head $ do
    signal <- signalCreate
    fmap Event . cachedIO . liftIO . runManagedT Nothing $ \occRef -> do
      handler <- managedWeakHandler $ writeIORef occRef . Just
      liftIO (f $ BrainTrigger signal handler) >>= managedFinalize
      pure $ Push
        { pushOccurrence = readIORef occRef
        , pushFired = signal
        , pushHeight = return 0
        , pushHeightInvalidated = signalNever
        }

instance MonadSample Brain Head where
  sample (Behavior b) = Head $ b >>= withDependent pullValue

instance MonadReflexHost Brain Head where
  fireEventsAndRead evs r = Head $ do
    liftIO $ F.forM_ evs $ \(t :=> v) -> triggerSetup t v
    runPropagate $ do
      F.forM_ evs $ \(t :=> v) -> signalEmit (triggerSignal t) v
      liftIO $ runRead r
  subscribeEvent = fmap BrainHandle . runPropagateHead . eventPush

holdPush :: a -> Dependent (Push a) -> IO (IO (), Behavior Brain a)
holdPush a pDep = do
  invalidated <- signalCreate
  let invalidate = signalEmit invalidated ()
  valueRef <- runManagedT a $ \valueRef -> do
    eventP <- managedDependent pDep
    managedConnect (pushFired eventP) $ \a' -> liftIO $ do
      writeIORef valueRef a'
      invalidate
    return valueRef
  pure (invalidate, Behavior . pure $ ffor valueRef $ \valRef -> Pull
    { pullValue = readIORef valRef
    , pullInvalidated = invalidated
    })

instance MonadHold Brain Head where
  hold a event = do
    p <- runPropagateHead $ eventPush event
    fmap snd $ liftIO $ holdPush a p

newtype EventM a = EventM { runEventM :: PropagateM a }
  deriving (Functor, Applicative, Monad)

instance MonadSample Brain EventM where
  sample (Behavior b) = EventM . liftIO $ b >>= withDependent pullValue

instance MonadHold Brain EventM where
  hold a event = EventM $ do
    p <- eventPush event
    (invalidate, b) <- liftIO $ holdPush a p
    occ <- liftIO $ withDependent pushOccurrence p
    when (isJust occ) $ scheduleInvalidate invalidate
    pure b

newtype BrainRead a = BrainRead { runRead :: IO a }
  deriving (Functor, Applicative, Monad)

instance MonadReadEvent Brain BrainRead where
  readEvent (BrainHandle h) = BrainRead $ fmap return <$> forDependent h pushOccurrence

instance MonadSample Brain BrainRead where
  sample (Behavior b) = BrainRead $ b >>= withDependent pullValue
