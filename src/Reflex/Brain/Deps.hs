{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
module Reflex.Brain.Deps
  ( Dependent(..), Deps(..)
  , dependOn
  , withDependent
  , forDependent

  , ManagedT()
  , managedDependOn
  , managedFinalize
  , managedWeakHandler
  , managedConnect
  , managedDependent
  , managedDynDependent
  , runManagedT
  ) where

import Control.Exception
import Data.IORef
import Control.Monad.Reader.Class
import Control.Monad.Cont.Class
import Control.Monad.Writer.Class
import Control.Monad.RWS.Class
import System.Mem.Weak
import Control.Monad.State.Strict
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Reflex.Brain.Signal

import qualified Data.Foldable as F

data Dependent a = Dependent
  { dependentDeps :: !Deps
  , dependentValue :: a
  } 

instance Functor Dependent where
  fmap f (Dependent x a) = Dependent x (f a)

instance Applicative Dependent where
  pure = Dependent None
  Dependent x f <*> Dependent y a = Dependent (Both x y) $ f a

instance Monad Dependent where
  return = pure
  Dependent x a >>= f = case f a of Dependent y b -> Dependent (Both x y) b

{-# INLINE dependOn #-}
dependOn :: a -> b -> Dependent a
a `dependOn` b = Dependent (Both b None) a

{-# INLINE withDependent #-}
withDependent :: MonadIO m => (a -> m b) -> Dependent a -> m b
withDependent f (Dependent d a) = do
  r <- f a
  liftIO $ touch d
  return r

{-# INLINE forDependent #-}
forDependent :: MonadIO m => Dependent a -> (a -> m b) -> m b
forDependent = flip withDependent

data Deps = forall a b. Both a b | None
newtype ManagedT m a = ManagedT { unManagedT :: (StateT (IO (), Deps) m a) }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadTrans, MonadIO
           ,MonadCont)
deriving instance MonadReader r m => MonadReader r (ManagedT m)
deriving instance MonadWriter w m => MonadWriter w (ManagedT m)
deriving instance MonadRWS r w s m => MonadRWS r w s (ManagedT m)
instance MonadState s m => MonadState s (ManagedT m) where
  get = lift get
  put = lift . put
  state = lift . state

{-# INLINE managedDependOn #-}
managedDependOn :: Monad m => a -> ManagedT m ()
managedDependOn a = ManagedT . modify $ fmap (Both a)

{-# INLINE managedFinalize #-}
managedFinalize :: Monad m => IO () -> ManagedT m ()
managedFinalize f = ManagedT . modify $ \(final, d) -> (final >> f, d)

{-# INLINE managedWeak #-}
managedWeak :: MonadIO m => a -> ManagedT m (Weak a)
managedWeak a = do
  a' <- liftIO $ evaluate a
  weak <- liftIO $ mkWeakPtr a' Nothing
  managedDependOn a'
  return weak

{-# INLINE managedWeakHandler #-}
managedWeakHandler :: (MonadIO f, MonadIO m) => (a -> f ()) -> ManagedT m (a -> f ())
managedWeakHandler f = do
  f' <- managedWeak f
  return $ \x -> liftIO (deRefWeak f') >>= F.mapM_ ($ x)

{-# INLINE managedConnect #-}
managedConnect :: (MonadIO m, MonadIO f) => Signal f a -> (a -> f ()) -> ManagedT m (IO ()) 
managedConnect sig f = do
  f' <- managedWeakHandler f
  disconnect <- liftIO $ signalConnect sig f'
  managedFinalize disconnect
  return disconnect

{-# INLINE managedDependent #-}
managedDependent :: Monad m => Dependent a -> ManagedT m a
managedDependent (Dependent deps a) = do
  managedDependOn deps
  return a

{-# INLINE managedDynDependent #-}
managedDynDependent :: (MonadIO f, Monad m) => f (Dependent a) -> ManagedT m (f a)
managedDynDependent f = do
  managedDependOn f
  return $ f >>= withDependent return 

{-# INLINE runManagedT #-}
runManagedT :: MonadIO m => b -> (IORef b -> ManagedT m a) -> m (Dependent a)
runManagedT b f = do
  ref <- liftIO $ newIORef b
  (val, (final, !deps)) <- runStateT (unManagedT $ f ref) (return (), None)
  liftIO . void $ mkWeakIORef ref final
  return $ Dependent (Both ref deps) val
