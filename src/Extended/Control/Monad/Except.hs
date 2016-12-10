{-# LANGUAGE MultiParamTypeClasses #-}
module Extended.Control.Monad.Except (
    module X
  , liftEither
  , unliftEither
    ) where

import           Control.Monad.Except as X

liftEither :: MonadError e m => Either e a -> m a
liftEither (Left e)  = throwError e
liftEither (Right r) = return r

unliftEither :: MonadError e m => m a -> m (Either e a)
unliftEither action =
    fmap Right action `catchError` (return . Left)
