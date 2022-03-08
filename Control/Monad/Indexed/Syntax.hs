-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Indexed.Syntax
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Alexis Williams <alexis@typedr.at>
-- Stability   :  experimental
--
----------------------------------------------------------------------------
module Control.Monad.Indexed.Syntax ( fmap, (<*>), (>>=), (>>), join, return, mfix, fail ) where

import qualified Control.Monad.Fail as Fail
import qualified Control.Monad.Fix  as Fix
import           Prelude            hiding ( fmap, (<*>), (=<<), (>>=), (>>), return, fail )

import Data.Functor.Indexed
import Control.Applicative.Indexed
import Control.Monad.Indexed

fmap :: (IxFunctor f) => (a -> b) -> f i j a -> f i j b
fmap = imap
{-# INLINE fmap #-}

infixl 4 <*>
(<*>) :: (IxApply f) => f i j (a -> b) -> f j k a -> f i k b
(<*>) = (<<.>>)
{-# INLINE (<*>) #-}

infixl 1 >>=
(>>=) :: (IxBind m) => m i j a -> (a -> m j k b) -> m i k b
(>>=) = (>>>-)
{-# INLINE (>>=) #-}

infixl 4 >>
(>>) :: (IxApply f) => f i j a -> f j k b -> f i k b
a >> b = const id <<$>> a <<.>> b
{-# INLINE (>>) #-}

join :: (IxBind m) => m i j (m j k a) -> m i k a
join = ijoin
{-# INLINE join #-}

return :: (IxApplicative f) => a -> f i i a
return = ipure
{-# INLINE return #-}

mfix :: (forall idx. Fix.MonadFix (m idx idx)) => (a -> m i i a) -> m i i a
mfix = Fix.mfix
{-# INLINE mfix #-}

fail :: (forall idx. Fail.MonadFail (m idx idx)) => String -> m i i a
fail = Fail.fail
{-# INLINE fail #-}
