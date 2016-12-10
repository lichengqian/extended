{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Extended.Control.Monad.RSE (
    module X
  , RSET(..)
  , evalRSET
  , runRSET
    ) where

import           Control.Applicative           as X
import           Control.Monad.Reader          as X
import           Control.Monad.State.Strict    as X
import           Extended.Control.Monad.Except as X

{-|
请参考 : https://github.com/nikita-volkov/strelka-core/blob/master/library/Strelka/Core/RequestParser.hs
-}
newtype RSET r s e m a =
  RSET (ReaderT r (StateT s (ExceptT e m)) a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadError e)

instance MonadTrans (RSET r s e) where
  lift m =
    RSET (lift (lift (lift m)))

evalRSET :: (Monad m) => RSET r s e m a -> r -> s -> m (Either e a)
evalRSET (RSET impl) request segments =
  runExceptT (evalStateT (runReaderT impl request) segments)

runRSET :: RSET r s e m a -> r -> s -> m (Either e (a, s))
runRSET (RSET impl) request segments =
  runExceptT (runStateT (runReaderT impl request) segments)
