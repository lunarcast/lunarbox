module Lunarbox.Data.ProjectId
  ( ProjectId(..)
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)

newtype ProjectId
  = ProjectId Int

derive instance eqProjectId :: Eq ProjectId

derive instance ordProjectID :: Ord ProjectId

derive instance newtypeProjectId :: Newtype ProjectId _

derive instance genericProjectId :: Generic ProjectId _

derive newtype instance encodeJsonProjectId :: EncodeJson ProjectId

derive newtype instance decodeJsonProjectId :: DecodeJson ProjectId

derive newtype instance showProjectId :: Show ProjectId
