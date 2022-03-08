-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Applicative.Indexed
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Alexis Williams <alexis@typedr.at>
-- Stability   :  experimental
--
----------------------------------------------------------------------------

module Control.Applicative.Indexed 
    ( -- * IxApply
      IxApply(..)
      -- * IxApplicative
    , IxApplicative(..)
    , (<<*>>), (*>>), (<<*)
    ) where

import Data.Functor.Indexed

-- | A strong lax semi-monoidal endofunctor on Hask ⨯ kᵏ. This is equivalent to an 'IxApplicative' without 'ipure'.
class (IxFunctor f) => IxApply f where
    infixl 4 <<.>>
    (<<.>>) :: f i j (a -> b) -> f j k a -> f i k b

    infixl 4 .>>
    (.>>) :: f i j a -> f j k b -> f i k b
    a .>> b = const id <<$>> a <<.>> b
    {-# INLINE (.>>) #-}

    infixl 4 <<.
    (<<.) :: f i j a -> f j k b -> f i k a
    a <<. b = const <<$>> a <<.>> b
    {-# INLINE (<<.) #-}

-- | Strong lax monoidal endofunctor on Hask ⨯ kᵏ.    
class (IxApply f, forall i. Applicative (f i i)) => IxApplicative f where
    ipure :: a -> f i i a

infixl 4 <<*>>
(<<*>>) :: (IxApply f) => f i j (a -> b) -> f j k a -> f i k b
(<<*>>) = (<<.>>)

infixl 4 *>>
(*>>) :: (IxApply f) => f i j a -> f j k b -> f i k b
(*>>) = (.>>)

infixl 4 <<*
(<<*) :: (IxApply f) => f i j a -> f j k b -> f i k a
(<<*) = (<<.)
