module Lunarbox.Data.Graph where

import Prelude
import Data.Graph (Graph, fromMap, toMap)
import Data.Graph as Graph
import Data.Map as Map
import Data.Set as Set
import Data.Tuple (Tuple(..), fst)
import Data.Unfoldable (class Unfoldable)

type MapGraphRep k v
  = Map.Map k (Tuple v (Set.Set k))

singleton :: forall k v. Ord k => k -> v -> Graph k v
singleton k v = fromMap $ Map.singleton k (Tuple v Set.empty)

keys :: forall k v u. Ord k => Unfoldable u => Graph k v -> u k
keys = Graph.toMap >>> Map.keys >>> Set.toUnfoldable

overMap :: forall k v. Ord k => (MapGraphRep k v -> MapGraphRep k v) -> Graph k v -> Graph k v
overMap mapper = toMap >>> mapper >>> fromMap

filter :: forall k v. Ord k => ((Tuple v (Set.Set k)) -> Boolean) -> Graph k v -> Graph k v
filter predicate = overMap $ Map.filter predicate

filterValues :: forall k v. Ord k => (v -> Boolean) -> Graph k v -> Graph k v
filterValues predicate = filter (fst >>> predicate)

entries :: forall k v u. Ord k => Unfoldable u => Graph k v -> u (Tuple k v)
entries = toMap >>> map fst >>> Map.toUnfoldable
