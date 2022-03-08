-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad,State.Indexed.Class
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Alexis Williams <alexis@typedr.at>
-- Stability   :  experimental
--
----------------------------------------------------------------------------
module Control.Monad.State.Indexed.Class
    ( -- * @IxMonadState@
      IxMonadState(..)
      -- * @IxMonadState'@, a kind-generalization of @IxMonadState@
    , IxMonadState'(..)
    ) where

import Control.Applicative.Indexed
import Control.Monad.Indexed
import Data.Coerce
import Data.Functor.Identity
import Data.Kind                    ( Type )

class (IxMonad m) => IxMonadState' (f :: k -> Type) (m :: k -> k -> Type -> Type) | m -> f where
    {-# MINIMAL istate' | iget', iput' #-}
    
    iget' :: m i i (f i)
    iget' = istate' (\i -> (i, i))
    
    iput' :: f j -> m i j ()
    iput' j = istate' (\_ -> ((), j))
    
    istate' :: (f i -> (a, f j)) -> m i j a
    istate' f = iget' >>>= \i ->
        let ~(a, j) = f i in iput' j *>> ipure a

class (IxMonadState' Identity m, forall i j x y. Coercible x y => Coercible (m i j x) (m i j y)) => IxMonadState m where
    iget :: forall i. m i i i
    iget = coerce (iget' :: m i i (Identity i))
    {-# INLINE iget #-}

    iput :: forall i j. j -> m i j ()
    iput = coerce (iput' :: Identity j -> m i j ())
    {-# INLINE iput #-}

    istate :: forall i j a. (i -> (a, j)) -> m i j a
    istate = coerce (istate' :: (Identity i -> (a, Identity j)) -> m i j a)
    {-# INLINE istate #-}
