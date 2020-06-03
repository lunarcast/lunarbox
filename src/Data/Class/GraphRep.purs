module Lunarbox.Data.Class.GraphRep where

import Prelude
import Data.Map (Map)
import Data.Set (Set)
import Data.Tuple (Tuple)
import Lunarbox.Data.Graph (Graph(..))

-- Generic typeclass for everything which can be represented as a graph
class GraphRep f k v | f -> k, f -> v where
  toGraph :: f -> Graph k v

instance graphRepMap :: GraphRep (Map k (Tuple v (Set k))) k v where
  toGraph = Graph

instance graphRepGraph :: GraphRep (Graph k v) k v where
  toGraph = identity
