module Lunarbox.Data.Utils
  ( decodeAt
  ) where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut (decodeJson, (.:), class DecodeJson)
import Data.Either (Either)

-- Decode a single field of some json
decodeAt :: forall a. DecodeJson a => String -> Json -> Either String a
decodeAt key = (_ .: key) <=< decodeJson
