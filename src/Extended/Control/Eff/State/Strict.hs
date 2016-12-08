{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Extended.Control.Eff.State.Strict (
    HasState
  , use
  , (%=)
  , (.=)
  , module Control.Eff.State.Strict
    ) where

import           Control.Eff
import           Control.Eff.State.Strict
import qualified Control.Lens             as Lens
import           Data.Map.Strict
import           Data.Typeable

type HasState s r = (Typeable s, Member (State s) r)

use :: HasState s r => Lens.Getting a s a -> Eff r a
use l = do
  v <- get
  return $ Lens.view l v
{-# INLINE use #-}

(%=) :: HasState s r => Lens.ASetter s s a b -> (a -> b) -> Eff r ()
l %= f = modify $ Lens.over l f
{-# INLINE (%=) #-}

(.=) :: HasState s r => Lens.ASetter s s a b -> b -> Eff r ()
l .= v = modify $ Lens.set l v
{-# INLINE (.=) #-}

infix 4 .=, %=
