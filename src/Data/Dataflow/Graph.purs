module Lunarbox.Data.Dataflow.Graph
  ( compileGraph
  ) where

import Prelude
import Data.Lens (view)
import Data.Lens.At (at)
import Data.List (catMaybes, foldr, reverse)
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(..))
import Lunarbox.Data.Dataflow.Class.Expressible (class Expressible, nullExpr, toExpression)
import Lunarbox.Data.Dataflow.Expression (Expression, VarName(..))
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..), letWithLocation)
import Lunarbox.Data.Graph (Graph, topologicalSort)

-- Takes a key and a graph and uses that to produce an Expression
compileGraphNode :: forall k v l. Ord k => Expressible v (Maybe l) => Graph k v -> k -> Maybe (Tuple k (Expression (ExtendedLocation k l)))
compileGraphNode graph key = Tuple key <$> map (maybe Nowhere $ DeepLocation key) <$> toExpression <$> view (at key) graph

-- Takes a graph of something and compiles it into an Expression
compileGraph :: forall k v l. Ord k => Eq l => Show k => Expressible v (Maybe l) => Graph k v -> Expression (ExtendedLocation k l)
compileGraph graph =
  let
    sorted =
      reverse
        $ topologicalSort
            graph

    emptyExpression = nullExpr Nowhere
  in
    foldr
      ( \(Tuple key value) body ->
          if body == emptyExpression then
            value
          else
            letWithLocation (Location key) (VarName $ show key) value body
      )
      emptyExpression
      $ catMaybes
      $ compileGraphNode graph
      <$> sorted
