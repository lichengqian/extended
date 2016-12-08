{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Extended.Control.Monad.Logger (
    module X
  , logDebugN
  , logInfoN
  , logWarnN
  , logErrorN
  , debugF, infoF, warnF, errorF
    ) where

import           Control.Monad.Logger   as X hiding (logDebugN, logErrorN,
                                              logInfoN, logWarnN)
import qualified Control.Monad.Logger   as Logger
import           Data.String.Conv
import           Data.Text
import qualified Data.Text.Lazy         as T
import qualified Data.Text.Lazy.Builder as T
import           Formatting             as X
import           Formatting.Internal

logDebugN, logInfoN, logWarnN, logErrorN :: (MonadLogger m, StringConv a Text) => a -> m ()
logDebugN = Logger.logDebugN . toS
logInfoN  = Logger.logInfoN . toS
logWarnN  = Logger.logWarnN . toS
logErrorN = Logger.logErrorN . toS

{-# INLINE logDebugN #-}
{-# INLINE logInfoN #-}
{-# INLINE logWarnN #-}
{-# INLINE logErrorN #-}

logF :: (MonadLogger m) => (Text -> m ()) -> Format (m ()) a -> a
logF out m = runFormat m (out . T.toStrict . T.toLazyText)

debugF, infoF, warnF, errorF :: (MonadLogger m) => Format (m ()) a -> a
debugF = logF Logger.logDebugN
infoF  = logF Logger.logInfoN
warnF  = logF Logger.logWarnN
errorF = logF Logger.logErrorN

