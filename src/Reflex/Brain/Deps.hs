{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
module Reflex.Brain.Deps
  ( Deps()
  , dep
  , addDep
  , WithDeps()
  , withDeps
  , discardDeps
  , useWithDeps
  ) where

import Foreign.StablePtr
import Data.Monoid

data Deps = Both !Deps !Deps | forall a. Add !Deps a | None
instance Monoid Deps where
  mempty = None
  mappend = Both

dep :: a -> Deps
dep = addDep None

addDep :: Deps -> a -> Deps
addDep d !a = Add d a

data WithDeps a = WithDeps Deps a

discardDeps :: WithDeps a -> a
discardDeps (WithDeps _ a) = a

{-# NOINLINE withDeps #-}
withDeps :: Deps -> a -> IO (WithDeps a)
withDeps !d = return . WithDeps d

{-# NOINLINE useWithDeps #-}
useWithDeps :: WithDeps a -> (a -> b) -> b
useWithDeps (WithDeps _ a) f = f a
