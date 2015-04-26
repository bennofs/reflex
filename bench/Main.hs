{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Monad.Ref
import Data.Functor.Misc
import Control.Monad.Primitive
import Control.Monad.IO.Class
import Data.Dependent.Sum
import Control.Concurrent.STM
import Control.Applicative
import System.IO.Unsafe
import Data.IORef
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad
import Reflex
import Reflex.Host.Class
import Reflex.Brain
import System.Mem
import System.IO
import Criterion.Main

import qualified Data.Traversable as T

import qualified Data.Dependent.Map as DM


main :: IO ()
main = defaultMain
  [ bgroup "Spider" microsSpider
  , bgroup "Brain"  microsBrain
  ]

instance NFData (IORef a) where
  rnf x = seq x ()

instance NFData (TVar a) where
  rnf x = seq x ()

newtype WHNF a = WHNF a
instance NFData (WHNF a) where
  rnf (WHNF a) = seq a ()

withSetup :: NFData b => String -> SpiderHost a -> (a -> SpiderHost b) -> Benchmark
withSetup name setup action = env (WHNF <$> runSpiderHost setup) $ \ ~(WHNF a) ->
  bench name . nfIO $ runSpiderHost (action a)

withSetupWHNF :: String -> SpiderHost a -> (a -> SpiderHost b) -> Benchmark
withSetupWHNF name setup action = env (WHNF <$> runSpiderHost setup) $ \ ~(WHNF a) ->
  bench name . whnfIO $ runSpiderHost (action a)

withSetupHeadWHNF :: String -> Head a -> (a -> Head b) -> Benchmark
withSetupHeadWHNF name setup action = env (WHNF <$> runHead setup) $ \ ~(WHNF a) ->
  bench name . whnfIO $ runHead (action a)

microsSpider :: [Benchmark]
microsSpider =
  [ bench "newIORef" $ whnfIO $ void $ newIORef ()
  , env (newIORef (42 :: Int)) (bench "readIORef" . whnfIO . readIORef)
  , bench "newTVar" $ whnfIO $ void $ newTVarIO ()
  , env (newTVarIO (42 :: Int)) (bench "readTVar" . whnfIO . readTVarIO)
  , bench "newEventWithTrigger" $ whnfIO . void $ runSpiderHost $ newEventWithTrigger $
      \trigger -> return () <$ evaluate trigger
  , bench "newEventWithTriggerRef" $ whnfIO . void $ runSpiderHost newEventWithTriggerRef
  , withSetupWHNF "subscribeEvent" newEventWithTriggerRef $ subscribeEvent . fst
  , withSetupWHNF "subscribeSwitch"
    (join $ hold <$> fmap fst newEventWithTriggerRef <*> fmap fst newEventWithTriggerRef)
    (subscribeEvent . switch)
  , withSetupWHNF "subscribeMerge(1)" (setupMerge 1) $ \(ev,_) -> subscribeEvent ev
  , withSetupWHNF "subscribeMerge(100)" (setupMerge 100) (subscribeEvent . fst)
  , withSetupWHNF "subscribeMerge(10000)" (setupMerge 10000) (subscribeEvent . fst)
  , bench "runHostFrame" $ whnfIO $ runSpiderHost $ runHostFrame $ return ()
  , withSetupWHNF "fireEventsAndRead(single/single)"
    (newEventWithTriggerRef >>= subscribePair)
    (\(subd, trigger) -> fireAndRead trigger (42 :: Int) subd)
  , withSetupWHNF "fireEventsOnly"
    (newEventWithTriggerRef >>= subscribePair)
    (\(subd, trigger) -> do
        Just key <- liftIO $ readIORef trigger
        fireEvents [key :=> (42 :: Int)])
  , withSetupWHNF "fireEventsAndRead(head/merge1)"
    (setupMerge 1 >>= subscribePair)
    (\(subd, t:riggers) -> fireAndRead t (42 :: Int) subd)
  , withSetupWHNF "fireEventsAndRead(head/merge100)"
    (setupMerge 100 >>= subscribePair)
    (\(subd, t:riggers) -> fireAndRead t (42 :: Int) subd)
  , withSetupWHNF "fireEventsAndRead(head/merge10000)"
      (setupMerge 10000 >>= subscribePair)
      (\(subd, t:riggers) -> fireAndRead t (42 :: Int) subd)
  , withSetupWHNF "fireEventsOnly(head/merge100)"
    (setupMerge 100 >>= subscribePair)
    (\(subd, t:riggers) -> do
        Just key <- liftIO $ readIORef t
        fireEvents [key :=> (42 :: Int)])
  , withSetupWHNF "hold"
    (do (ev,_) <- newEventWithTriggerRef
        subscribeEvent ev
        return ev) $
     hold (42 :: Int)
  , withSetupWHNF "sample"
    (do (ev,_) <- newEventWithTriggerRef
        subscribeEvent ev
        hold (42 :: Int) ev)
    sample    
  ]

microsBrain :: [Benchmark]
microsBrain =
  [ bench "newEventWithTrigger" $ whnfIO . void $ runHead $ newEventWithTrigger $
      \trigger -> return () <$ evaluate trigger
  , bench "newEventWithTriggerRef" $ whnfIO . void $ runHead newEventWithTriggerRef
  , withSetupHeadWHNF "subscribeEvent" newEventWithTriggerRef $ subscribeEvent . fst
  , withSetupHeadWHNF "subscribeMerge(1)" (setupMerge 1) $ \(ev,_) -> subscribeEvent ev
  , withSetupHeadWHNF "subscribeMerge(100)" (setupMerge 100) (subscribeEvent . fst)
  , withSetupHeadWHNF "subscribeMerge(10000)" (setupMerge 10000) (subscribeEvent . fst)
  , withSetupHeadWHNF "fireEventsAndRead(single/single)"
    (newEventWithTriggerRef >>= subscribePair)
    (\(subd, trigger) -> fireAndRead trigger (42 :: Int) subd)
  , withSetupHeadWHNF "fireEventsOnly"
    (newEventWithTriggerRef >>= subscribePair)
    (\(subd, trigger) -> do
        Just key <- liftIO $ readIORef trigger
        fireEvents [key :=> (42 :: Int)])
  , withSetupHeadWHNF "fireEventsAndRead(head/merge1)"
    (setupMerge 1 >>= subscribePair)
    (\(subd, t:riggers) -> fireAndRead t (42 :: Int) subd)
  , withSetupHeadWHNF "fireEventsAndRead(head/merge100)"
    (setupMerge 100 >>= subscribePair)
    (\(subd, t:riggers) -> fireAndRead t (42 :: Int) subd)
  , withSetupHeadWHNF "fireEventsAndRead(head/merge10000)"
      (setupMerge 10000 >>= subscribePair)
      (\(subd, t:riggers) -> fireAndRead t (42 :: Int) subd)
  , withSetupHeadWHNF "fireEventsOnly(head/merge100)"
    (setupMerge 100 >>= subscribePair)
    (\(subd, t:riggers) -> do
        Just key <- liftIO $ readIORef t
        fireEvents [key :=> (42 :: Int)])
  , withSetupHeadWHNF "hold"
    (do (ev,_) <- newEventWithTriggerRef
        subscribeEvent ev
        runPropagateHead $ eventPush ev) $
     liftIO . holdPush (42 :: Int)
  , withSetupHeadWHNF "sample"
    (do (ev,_) <- newEventWithTriggerRef
        subscribeEvent ev
        r <- hold (42 :: Int) ev
        r <$ sample r)
    sample    
  ]

{-# INLINE setupMerge #-}
setupMerge :: (Reflex t, MonadReflexHost t m, Ref m ~ Ref IO, Functor m, MonadRef m) => Int
           -> m (Event t (DM.DMap (Const2 Int a)),
                [IORef (Maybe (EventTrigger t a))])
setupMerge num = do
  (evs, triggers) <- unzip <$> replicateM 100 newEventWithTriggerRef
  let !m = DM.fromList [WrapArg (Const2 i) :=> v | (i,v) <- zip [0..] evs]
  return (merge m, triggers)

{-# INLINE subscribePair #-}
subscribePair :: (Reflex t, MonadReflexHost t m, Functor m) => (Event t a, b) -> m (EventHandle t a, b)
subscribePair (ev, b) = (,b) <$> subscribeEvent ev


{-# INLINE fireAndRead #-}
fireAndRead :: (MonadRef m, MonadReflexHost t m) => Ref m (Maybe (EventTrigger t a)) -> a
            -> EventHandle t b -> m (Maybe b)
fireAndRead trigger val subd = do
  Just key <- readRef trigger
  fireEventsAndRead [key :=> val] $ readEvent subd >>= T.sequence
