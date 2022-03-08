-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Indexed.
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Alexis Williams <alexis@typedr.at>
-- Stability   :  experimental
--
----------------------------------------------------------------------------
module Control.Monad.Trans.Indexed ( IxMonadTrans(..) ) where

import Control.Monad.Indexed

class (forall m. Monad m => IxMonad (t m)) => IxMonadTrans t where
    ilift :: m a -> t m i i a
