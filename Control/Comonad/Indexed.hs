-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Indexed
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Alexis Williams <alexis@typedr.at>
-- Stability   :  experimental
--
----------------------------------------------------------------------------
module Control.Comonad.Indexed 
    ( -- * IxExtend
      IxExtend(..)
      -- * IxComonad
    , IxComonad(..)
    ) where

import Control.Comonad
import Data.Functor.Indexed

class (IxFunctor w) => IxExtend w where
    {-# MINIMAL iduplicate | iextend #-}

    iduplicate :: w k i a -> w j i (w k j a)
    iduplicate = iextend id
    {-# INLINABLE iduplicate #-}

    iextend :: (w k j a -> b) -> w k i a -> w j i b
    iextend f = imap f . iduplicate
    {-# INLINABLE iextend #-}

class (IxExtend w, forall i. Comonad (w i i)) => IxComonad w where
    iextract :: w i i a -> a
