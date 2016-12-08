{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Extended.Control.Eff.Reader.Strict (
    module X
  , HasReader
  , view
    ) where

import           Control.Eff               as X
import           Control.Eff.Reader.Strict as X
import qualified Control.Lens              as Lens
import           Data.Typeable

type HasReader e r = (Typeable e, Member (Reader e) r)

view :: HasReader e r => Lens.Getting a e a -> Eff r a
view l = do
  v <- ask
  return $ Lens.view l v
{-# INLINE view #-}
