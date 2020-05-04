module Lunarbox.Data.ProjectId
  ( ProjectId(..)
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)

newtype ProjectId
  = ProjectId String

derive instance eqProjectId :: Eq ProjectId

derive instance ordProjectID :: Ord ProjectId

derive instance newtypeProjectId :: Newtype ProjectId _

derive instance genericProjectId :: Generic ProjectId _

derive newtype instance encodeJsonProjectId :: EncodeJson ProjectId

derive newtype instance decodeJsonProjectId :: DecodeJson ProjectId

instance showProjectId :: Show ProjectId where
  show = genericShow
