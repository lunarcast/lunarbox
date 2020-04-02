module Lunarbox.Data.Editor.ExtendedLocation
  ( ExtendedLocation(..)
  , _ExtendedLocation
  , _LocationExtensionWithDefault
  , _LocationExtension
  ) where

import Prelude
import Data.Lens (Lens', Prism', iso, prism')
import Data.Maybe (Maybe(..))

-- This represents a location which may or may not have an extra layer
data ExtendedLocation l l'
  = DeepLocation l l'
  | Location l

derive instance eqExtendedLocation :: (Eq l, Eq l') => Eq (ExtendedLocation l l')

derive instance ordExtendedLocation :: (Ord l, Ord l') => Ord (ExtendedLocation l l')

instance showExtendedLocation :: (Show l, Show l') => Show (ExtendedLocation l l') where
  show (Location l) = show l
  show (DeepLocation l l') = show l <> " -> " <> show l'

-- Lenses
_LocationExtensionWithDefault :: forall l l'. l -> Prism' (ExtendedLocation l l') l'
_LocationExtensionWithDefault default =
  prism' (DeepLocation default) case _ of
    DeepLocation _ l -> Just l
    Location _ -> Nothing

_LocationExtension :: forall l l'. Monoid l => Prism' (ExtendedLocation l l') l'
_LocationExtension = _LocationExtensionWithDefault mempty

_ExtendedLocation :: forall l l'. Lens' (ExtendedLocation l l') l
_ExtendedLocation =
  (flip iso) Location case _ of
    Location l -> l
    DeepLocation l _ -> l
