{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
module Extended.Control.Eff.Lift (
    module X
  , HasLift
  , HasMonadIO
    ) where

import           Control.Eff            as X
import           Control.Eff.Lift       as X
import           Control.Monad.Catch    as X
import           Control.Monad.IO.Class as X
import           Control.Monad.Logger
import           Data.Typeable

type HasLift m r = (Typeable m, SetMember Lift (Lift m) r)

type HasMonadIO m r = (MonadIO m, HasLift m r)

instance (Typeable m, MonadThrow m, SetMember Lift (Lift m) r) => MonadThrow (Eff r) where
    throwM = lift . throwM
    {-# INLINE throwM #-}

instance (Typeable m, MonadLogger m, SetMember Lift (Lift m) r) => MonadLogger (Eff r) where
    monadLoggerLog loc src level msg = lift $ monadLoggerLog loc src level msg
    {-# INLINE monadLoggerLog #-}
