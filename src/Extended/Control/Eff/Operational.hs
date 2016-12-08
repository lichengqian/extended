{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
module Extended.Control.Eff.Operational (
    module X
  , HasProgram
    ) where

import           Control.Eff             as X
import           Control.Eff.Operational as X
import           Data.Typeable

type HasProgram instr r = (Typeable instr, Member (Program instr) r)
