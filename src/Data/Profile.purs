module Lunarbox.Data.Profile
  ( Username(..)
  , Email(..)
  , Profile(..)
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)

newtype Email
  = Email String

derive instance newtypeEmail :: Newtype Email _

derive instance genericEmail :: Generic Email _

derive instance eqEmail :: Eq Email

derive instance ordEmail :: Ord Email

derive newtype instance encodeJsonEmail :: EncodeJson Email

derive newtype instance decodeJsonEmail :: DecodeJson Email

instance showEmail :: Show Email where
  show = genericShow

newtype Username
  = Username String

derive instance newtypeUsername :: Newtype Username _

derive instance genericUsername :: Generic Username _

derive instance eqUsername :: Eq Username

derive instance ordUsername :: Ord Username

derive newtype instance encodeJsonUsername :: EncodeJson Username

derive newtype instance decodeJsonUsername :: DecodeJson Username

instance showUsername :: Show Username where
  show = genericShow

newtype Profile
  = Profile
  { username :: Username
  , email :: Email
  }

derive instance newtypeProfile :: Newtype Profile _

derive instance genericProfile :: Generic Profile _

derive instance eq :: Eq Profile

derive instance ord :: Ord Profile

derive newtype instance encodeJsonProfile :: EncodeJson Profile

derive newtype instance decodeJsonProfile :: DecodeJson Profile

instance showProfile :: Show Profile where
  show = genericShow
