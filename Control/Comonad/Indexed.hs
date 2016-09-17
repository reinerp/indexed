-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Indexed
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Reiner Pope <reiner.pope@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Comonad.Indexed
  ( IxFunctor(..)
  , IxCopointed(..)
  , IxComonad(..)
  , iduplicate
  ) where

import Data.Functor.Indexed

class IxCopointed w => IxComonad w where
  iextend :: (w j k a -> b) -> w i k a -> w i j b

iduplicate :: IxComonad w => w i k a -> w i j (w j k a)
iduplicate = iextend id

