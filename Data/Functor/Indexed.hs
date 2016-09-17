-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Indexed
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Reiner Pope <reiner.pope@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Functor.Indexed
  ( IxFunctor(..)
  , IxCopointed(..)
  , IxPointed(..)
  , IxApplicative(..)
  , (<<$>>), (<<*>>), (<<*), (*>>)
  ) where

class IxFunctor f where
  imap :: (a -> b) -> f j k a -> f j k b

-- | Infix alias of 'imap'.  Or, ('<$>') for 'IxFunctor'.  Should be
-- interchangeable with ('<$>'), but requires 'IxFunctor' constraints instead
-- of (possibly many) 'Functor' constraints.
infixl 4 <<$>>
(<<$>>) :: IxFunctor f => (a -> b) -> f j k a -> f j k b
(<<$>>) = imap

class IxPointed m => IxApplicative m where
  iap :: m i j (a -> b) -> m j k a -> m i k b

-- | Infix alias of 'iap'.  Or, ('<*>') for 'IxApplicative'.
infixl 4 <<*>>
(<<*>>) :: IxApplicative f => f i j (a -> b) -> f j k a -> f i k b
(<<*>>) = iap

-- | ('Control.Applicative.<*') for 'IxApplicative'.
infixl 4 <<*
(<<*) :: IxApplicative f => f i j a -> f j k b -> f i k a
(<<*) a b = imap const a <<*>> b

-- | ('Control.Applicative.*>') for 'IxApplicative'.
infixl 4 *>>
(*>>) :: IxApplicative f => f i j a -> f j k b -> f i k b
(*>>) a b = imap (const id) a <<*>> b

class IxFunctor m => IxPointed m where
  ireturn :: a -> m i i a

class IxFunctor w => IxCopointed w where
  iextract :: w i i a -> a
