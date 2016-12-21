module Extended.Data.Aeson (
    module X
  , eitherFromJSON
  , addField
  , parseField
    ) where

import           Data.Aeson           as X
import           Data.Aeson.Casing    as X
import           Data.Aeson.Lens      as X
import           Data.Aeson.Types
import           Data.Aeson.Unit      as X
import           Data.Aeson.WithField as X
import qualified Data.HashMap.Strict  as M
import           Data.Text

eitherFromJSON :: FromJSON a => Value -> Either String a
eitherFromJSON v = case fromJSON v of
    Success a -> Right a
    Error s   -> Left s

parseField :: (FromJSON a) => Text -> Value -> Maybe a
parseField n (Object obj) = parseMaybe (.: n) obj

addField :: (ToJSON a) => Text -> a -> Value -> Value
addField n v (Object obj) = Object $ M.insert n (toJSON v) obj
