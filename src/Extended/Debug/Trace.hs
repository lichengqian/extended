{-# LANGUAGE CPP #-}
module Extended.Debug.Trace (
    module X
    ) where

#ifdef NO_TRACE
import           Debug.NoTrace as X
#else
import           Debug.Trace   as X
#endif
