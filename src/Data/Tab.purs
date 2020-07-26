module Lunarbox.Data.Tab where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Tab
  = Settings
  | Add
  | Tree
  | Problems

derive instance eqTab :: Eq Tab

derive instance genericTab :: Generic Tab _

instance showTab :: Show Tab where
  show = genericShow

instance encodeJsonTab :: EncodeJson Tab where
  encodeJson = genericEncodeJson

instance decodeJsonTab :: DecodeJson Tab where
  decodeJson = genericDecodeJson

-- Return the icon for a Tab
-- I could use a show instance
-- but this is more explicit I think
tabIcon :: Tab -> String
tabIcon = case _ of
  Settings -> "settings"
  Add -> "add"
  Tree -> "account_tree"
  Problems -> "error"
