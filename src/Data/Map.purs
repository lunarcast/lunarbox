module Lunarbox.Data.Map (maybeBimap) where

import Prelude
import Data.Array (catMaybes)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple, uncurry)

-- A combination of bimap and mapMaybeWithKeys
maybeBimap :: forall k v k' v'. Ord k => Ord k' => (k -> v -> Maybe (Tuple k' v')) -> Map.Map k v -> Map.Map k' v'
maybeBimap mapper = Map.fromFoldable <<< catMaybes <<< map (uncurry mapper) <<< Map.toUnfoldable
