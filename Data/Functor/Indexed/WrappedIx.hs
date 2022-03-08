-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Indexed.WrappedIx
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Alexis Williams <alexis@typedr.at>
-- Stability   :  experimental
--
----------------------------------------------------------------------------
module Data.Functor.Indexed.WrappedIx ( WrappedIx(..) ) where

import Data.Functor.Indexed
import Data.Kind                   ( Type )
import Control.Applicative.Indexed
import Control.Comonad
import Control.Comonad.Indexed
import Control.Monad.Fail          as Fail
import Control.Monad.Indexed
import Control.Monad.Trans.Indexed

newtype WrappedIx (f :: k -> k -> Type -> Type) i a = WrappedIx { unwrapIx :: f i i a }

deriving instance (IxFunctor f) => Functor (WrappedIx f i)

instance (IxApplicative f) => Applicative (WrappedIx f i) where
    (WrappedIx f) <*> (WrappedIx x) = WrappedIx $ f <<*>> x
    {-# INLINE (<*>) #-}

    pure = WrappedIx . ipure
    {-# INLINE pure #-}

instance (IxComonad w) => Comonad (WrappedIx w i) where
    extract = iextract . unwrapIx
    {-# INLINE extract #-}

    extend f (WrappedIx w) = WrappedIx $ iextend (f . WrappedIx) w
    {-# INLINE extend#-}
    
    duplicate (WrappedIx w) = WrappedIx . imap WrappedIx . iduplicate $ w
    {-# INLINE duplicate #-}

instance (IxMonad m) => Monad (WrappedIx m i) where
    (WrappedIx m) >>= f = WrappedIx . (m >>>-) . (unwrapIx .) $ f
    {-# INLINE (>>=) #-}

instance (IxMonadTrans t, MonadFail m) => MonadFail (WrappedIx (t m) i) where
    fail = WrappedIx . ilift . Fail.fail
    {-# INLINE fail #-}
