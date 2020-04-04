module Lunarbox.Data.Editor.ExtendedLocation
  ( ExtendedLocation(..)
  , letWithLocation
  , _ExtendedLocation
  , _LocationExtensionWithDefault
  , _LocationExtension
  ) where

import Prelude
import Data.Lens (Prism', prism')
import Data.List ((:), List(..))
import Data.Maybe (Maybe(..))
import Lunarbox.Data.Dataflow.Expression (Expression(..), VarName)

-- This represents a location which may or may not have an extra or a missing layer 
data ExtendedLocation l l'
  = DeepLocation l l'
  | Location l
  | Nowhere

derive instance eqExtendedLocation :: (Eq l, Eq l') => Eq (ExtendedLocation l l')

derive instance ordExtendedLocation :: (Ord l, Ord l') => Ord (ExtendedLocation l l')

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

_LocationExtension :: forall l l'. Monoid l => Prism' (ExtendedLocation l l') l'
_LocationExtension = _LocationExtensionWithDefault mempty

_ExtendedLocation :: forall l l'. Prism' (ExtendedLocation l l') l
_ExtendedLocation =
  prism' Location case _ of
    Location l -> Just l
    DeepLocation l _ -> Just l
    Nowhere -> Nothing

-- helpers
letWithLocation ::
  forall l l'.
  ExtendedLocation l l' ->
  VarName ->
  Expression (ExtendedLocation l l') ->
  Expression (ExtendedLocation l l') ->
  Expression (ExtendedLocation l l')
letWithLocation location name value body =
  Let Nowhere
    name
    value
    $ Chain Nowhere
    $ (Variable location name : body : Nil)
