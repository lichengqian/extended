module Extended.Data.Aeson (
    module X
  , eitherFromJSON
    ) where

import           Data.Aeson      as X
import           Data.Aeson.Lens as X

eitherFromJSON :: FromJSON a => Value -> Either String a
eitherFromJSON v = case fromJSON v of
    Success a -> Right a
    Error s   -> Left s
