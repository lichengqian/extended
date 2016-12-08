module Extended.Network.Socket (
    module Network.Socket
  , getNumericHostAddress
    ) where

import           Control.Exception     (throwIO)
import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as S8
import           Network.Socket

-- | Convert a host name (e.g. \"localhost\" or \"127.0.0.1\") to a
-- numeric host address (e.g. \"127.0.0.1\").
getNumericHostAddress :: S.ByteString -> IO S.ByteString
getNumericHostAddress host = do
    ais <- getAddrInfo Nothing (Just (S8.unpack host)) Nothing
    case ais of
        [] -> unsupportedAddressError
        (ai:_) -> do
            ni <- getNameInfo [NI_NUMERICHOST] True False (addrAddress ai)
            case ni of
                (Just numericHost, _) -> return $! S8.pack numericHost
                _                     -> unsupportedAddressError
  where
    unsupportedAddressError = throwIO $
        userError $ "unsupported address: " ++ S8.unpack host

