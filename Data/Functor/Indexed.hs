-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Indexed
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Alexis Williams <alexis@typedr.at>
-- Stability   :  experimental
--
----------------------------------------------------------------------------
module Data.Functor.Indexed
    ( IxFunctor, imap, (<<$>>), (<<$), ($>>), ivoid ) where

import Data.Functor
import Data.Kind    ( Type )

-- | An endofunctor on Hask ⨯ kᵏ sometimes precomposed with — → — ⨯ id_kᵏ.
--
--   At times, we elide the difference between an actual endofunctor and a
--   functor from Hask to Hask ⨯ kᵏ, because endomorphisms on k might not have
--   term-level witnesses, and so we assume (for common sense's sake) that when
--   we need to conjure one from thin air that it will always be id_kᵏ.
class (forall i j. Functor (f i j)) => IxFunctor (f :: k -> k -> Type -> Type)

-- | 'fmap' for 'IxFunctor's.
imap :: (IxFunctor f) => (a -> b) -> f i j a -> f i j b
imap = fmap
{-# INLINE imap #-}

-- | ('<$') for 'IxFunctor's.
infixl 4 <<$
(<<$) :: (IxFunctor f) => a -> f i j b -> f i j a
(<<$) = (<$)
{-# INLINE (<<$) #-}

infixl 4 $>>
($>>) :: (IxFunctor f) => f i j a -> b -> f i j b
($>>) = ($>)
{-# INLINE ($>>) #-}

-- | Infix alias of 'imap', or ('<$>') for 'IxFunctor's. Interchangeable with ('<$>').
infixl 4 <<$>>
(<<$>>) :: IxFunctor f => (a -> b) -> f i j a -> f i j b
(<<$>>) = imap
{-# INLINE (<<$>>) #-}

-- | 'ivoid' for 'IxFunctor's.
ivoid :: (IxFunctor f) => f i j a -> f i j ()
ivoid = void
{-# INLINE ivoid #-}
