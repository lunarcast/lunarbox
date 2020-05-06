module Lunarbox.Data.Editor.ExtendedLocation
  ( ExtendedLocation(..)
  , normalize
  , nothing
  , _ExtendedLocation
  , _LocationExtensionWithDefault
  , _LocationExtension
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Lunarbox.Data.Dataflow.Expression (Expression(..))

-- This represents a location which may or may not have an extra or a missing layer 
data ExtendedLocation l l'
  = DeepLocation l l'
  | Location l
  | Nowhere

derive instance eqExtendedLocation :: (Eq l, Eq l') => Eq (ExtendedLocation l l')

derive instance ordExtendedLocation :: (Ord l, Ord l') => Ord (ExtendedLocation l l')

derive instance genericExtendedLocation :: Generic (ExtendedLocation l l') _

instance encodeJsonExtendedLocation :: (EncodeJson l, EncodeJson l') => EncodeJson (ExtendedLocation l l') where
  encodeJson = genericEncodeJson

instance decodeJsonExtendedLocation :: (DecodeJson l, DecodeJson l') => DecodeJson (ExtendedLocation l l') where
  decodeJson = genericDecodeJson

instance defaultExtendedLocation :: Default (ExtendedLocation l l') where
  def = Nowhere

instance showExtendedLocation :: (Show l, Show l') => Show (ExtendedLocation l l') where
  show (Location l) = show l
  show (DeepLocation l l') = show l <> " -> " <> show l'
  show Nowhere = "-"

-- Lenses
_LocationExtensionWithDefault :: forall l l'. l -> Prism' (ExtendedLocation l l') l'
_LocationExtensionWithDefault default =
  prism' (DeepLocation default) case _ of
    DeepLocation _ l -> Just l
    _ -> Nothing

_LocationExtension :: forall l l'. Default l => Prism' (ExtendedLocation l l') l'
_LocationExtension = _LocationExtensionWithDefault def

_ExtendedLocation :: forall l l'. Prism' (ExtendedLocation l l') l
_ExtendedLocation =
  prism' Location case _ of
    Location l -> Just l
    DeepLocation l _ -> Just l
    Nowhere -> Nothing

-- Normalize nested Locations
normalize :: forall l l' l''. ExtendedLocation l (ExtendedLocation l' l'') -> ExtendedLocation l (ExtendedLocation l' l'')
normalize = case _ of
  DeepLocation l Nowhere -> Location l
  location -> location

nothing :: forall l l'. Expression (ExtendedLocation l l')
nothing = TypedHole Nowhere
