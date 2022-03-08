-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad,State.Indexed.Strict
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Alexis Williams <alexis@typedr.at>
-- Stability   :  experimental
--
----------------------------------------------------------------------------
module Control.Monad.State.Indexed.Strict
    ( -- * @IxState@
      IxState, runIxState
      -- * @IxStateT@
    , IxMonadState(..), IxStateT, runIxStateT
      -- * @IxStateT'@, a kind-generalization of @IxStateT@
    , IxMonadState'(..), IxStateT', runIxStateT'
    ) where

import Control.Applicative.Indexed
import Control.Monad.Indexed
import Control.Monad.State.Indexed.Class
import Data.Coerce
import Data.Functor.Identity
import Data.Functor.Indexed
import Data.Functor.Indexed.WrappedIx

newtype IxStateT' f m i j a = IxStateT' { runIxStateT' :: (f i -> m (a, f j)) }

type IxStateT = IxStateT' Identity

runIxStateT :: forall i j m a. (forall x y. Coercible x y => Coercible (m x) (m y)) => IxStateT m i j a -> i -> m (a, j)
runIxStateT = coerce (runIxStateT' :: IxStateT' Identity m i j a -> Identity i -> m (a, Identity j))

type IxState = IxStateT Identity

runIxState :: IxState i j a -> i -> (a, j)
runIxState = coerce . runIxStateT

deriving instance (Functor m) => Functor (IxStateT' f m i j)
instance (Functor m) => IxFunctor (IxStateT' f m)

instance (Monad m) => IxApply (IxStateT' f m) where
    IxStateT' mf <<.>>  IxStateT' mx = IxStateT' $ \i -> do
        (f, j) <- mf i
        (x, k) <- mx j
        pure (f x, k)
    {-# INLINE (<<.>>) #-}

instance (Monad m) => IxApplicative (IxStateT' f m) where
    ipure a = IxStateT' $ \s -> pure (a, s)
    {-# INLINE ipure #-}

deriving via (WrappedIx (IxStateT' f m) i) instance (Monad m) => Applicative (IxStateT' f m i i)

instance (Monad m) => IxBind (IxStateT' f m) where
    m >>>- x = IxStateT' $ \i -> do
        (a, j) <- runIxStateT' m i
        runIxStateT' (x a) j
    {-# INLINE (>>>-) #-}

instance (Monad m) => IxMonad (IxStateT' f m)

deriving via (WrappedIx (IxStateT' f m) i) instance (Monad m) => Monad (IxStateT' f m i i)

instance (Monad m) => IxMonadState' f (IxStateT' f m) where
    istate' f = IxStateT' (pure . f) 
    {-# INLINE istate' #-}

instance (Monad m, forall x y. Coercible x y => Coercible (m x) (m y)) => IxMonadState (IxStateT m)
