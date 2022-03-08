-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Indexed
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Alexis Williams <alexis@typedr.at>
-- Stability   :  experimental
--
----------------------------------------------------------------------------
module Control.Monad.Indexed 
    ( -- * @IxBind@
      IxBind(..)
    , (-<<<)
    , ijoin
      -- * @IxMonad@
    , IxMonad
    , (>>>=), (=<<<)
    ) where

import Control.Applicative.Indexed

class IxApply m => IxBind m where
    infixl 1 >>>-
    (>>>-) :: m i j a -> (a -> m j k b) -> m i k b

infixr 1 -<<<
(-<<<) :: (IxBind m) => (a -> m j k b) -> m i j a -> m i k b
(-<<<) = flip (>>>-)
{-# INLINE (-<<<) #-}

ijoin :: (IxBind m) => m i j (m j k a) -> m i k a
ijoin = (>>>- id)
{-# INLINE ijoin #-}

class (IxBind m, IxApplicative m, forall i. Monad (m i i)) => IxMonad m

infixl 1 >>>=
(>>>=) :: (IxMonad m) => m i j a -> (a -> m j k b) -> m i k b
(>>>=) = (>>>-)
{-# INLINE (>>>=) #-}

infixr 1 =<<<
(=<<<) :: (IxMonad m) => (a -> m j k b) -> m i j a -> m i k b
(=<<<) = (-<<<)
{-# INLINE (=<<<) #-}
