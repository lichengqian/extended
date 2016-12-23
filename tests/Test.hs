{-# LANGUAGE TemplateHaskell #-}
module Test where

import           Control.Monad.Reader
import           Extended.Language.Haskell.TH

class (Monad m) => MonadExt m where
    extLog :: Int -> m ()

liftReaderT ''MonadExt

{-
======>
    instance MonadExt m => MonadExt (ReaderT a m) where
      {-# INLINE extLog #-}
      extLog p_aFY7 = lift (extLog p_aFY7)
-}
