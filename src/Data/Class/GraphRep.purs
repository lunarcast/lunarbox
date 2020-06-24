module Lunarbox.Data.Class.GraphRep where

import Prelude
import Data.Map (Map)
import Data.Tuple (Tuple(..))
import Lunarbox.Data.Editor.Class.Depends (class Depends, getDependencies)
import Lunarbox.Data.Graph (Graph(..), invert)

-- Generic typeclass for everything which can be represented as a graph
class GraphRep f k v | f -> k, f -> v where
  toGraph :: f -> Graph k v

instance graphRepDependencyMap :: (Ord k, Depends v k) => GraphRep (Map k v) k v where
  toGraph functions = invert $ Graph $ go <$> functions
    where
    go function = Tuple function $ getDependencies function

instance graphRepGraph :: GraphRep (Graph k v) k v where
  toGraph = identity
