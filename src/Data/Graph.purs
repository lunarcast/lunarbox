module Lunarbox.Data.Graph where

import Prelude
import Data.Graph (Graph, fromMap)
import Data.Map as Map
import Data.Set as Set
import Data.Tuple (Tuple(..))

singleton :: forall k v. Ord k => k -> v -> Graph k v
singleton k v = fromMap $ Map.singleton k (Tuple v Set.empty)
