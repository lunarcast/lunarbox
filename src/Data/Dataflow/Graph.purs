module Lunarbox.Data.Dataflow.Graph
  ( ExtendedLocation(..)
  , compileGraph
  ) where

import Prelude
import Data.Lens (view)
import Data.Lens.At (at)
import Data.List (catMaybes, foldr)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Lunarbox.Data.Dataflow.Class.Expressible (class Expressible, nullExpr, toExpression)
import Lunarbox.Data.Dataflow.Expression (Expression(..), VarName(..))
import Lunarbox.Data.Graph (Graph, topologicalSort)

-- This represents a location which may or may not have an extra layer
data ExtendedLocation l l'
  = DeepLocation l l'
  | Location l

-- Takes a key and a graph and uses that to produce an Expression
compileGraphNode :: forall k v l. Ord k => Expressible v l => Graph k v -> k -> Maybe (Tuple k (Expression (ExtendedLocation k l)))
compileGraphNode graph key = Tuple key <$> map (DeepLocation key) <$> toExpression <$> view (at key) graph

-- Takes a graph of something and compiles it into an Expression
compileGraph :: forall k v l. Ord k => Show k => Expressible v l => k -> Graph k v -> Expression (ExtendedLocation k l)
compileGraph caseEmpty graph =
  let
    sorted =
      topologicalSort
        graph
  in
    foldr
      (\(Tuple key value) body -> Let (Location key) (VarName $ show key) value body)
      (nullExpr $ Location caseEmpty)
      $ catMaybes
      $ compileGraphNode graph
      <$> sorted
