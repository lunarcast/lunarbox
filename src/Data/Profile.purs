module Lunarbox.Data.Profile
  ( Username(..)
  , Email(..)
  , ProfileRep
  , Profile
  , ProfileWithEmail
  , _username
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))

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

-- The actual type for profiles
type ProfileRep row
  = ( username :: Username
    , isAdmin :: Boolean
    | row
    )

type Profile
  = { | ProfileRep () }

type ProfileWithEmail
  = { | ProfileRep ( email :: Email ) }

-- Lenses
_username :: forall r. Lens' { username :: Username | r } Username
_username = prop (SProxy :: _ "username")
