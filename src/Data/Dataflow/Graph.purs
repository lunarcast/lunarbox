module Lunarbox.Data.Dataflow.Graph
  ( compileGraph
  ) where

import Prelude
import Data.Lens (view)
import Data.Lens.At (at)
import Data.List (catMaybes, foldr)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Lunarbox.Data.Dataflow.Expression (Expression(..), VarName(..), inputs, wrap)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..))
import Lunarbox.Data.Graph (Graph, topologicalSort)

-- Takes a key and a graph and uses that to produce an Expression
compileGraphNode :: forall k v l. Ord k => (v -> Expression l) -> Graph k v -> k -> Maybe (Tuple k (Expression (ExtendedLocation k l)))
compileGraphNode toExpression graph key = Tuple key <$> wrap (Location key) <$> map (DeepLocation key) <$> toExpression <$> view (at key) graph

-- Takes a graph of something and compiles it into an Expression
compileGraph :: forall k v l. Ord k => Eq l => Show k => Show l => (v -> Expression l) -> Graph k v -> k -> Expression (ExtendedLocation k l)
compileGraph toExpression graph main =
  let
    sorted =
      topologicalSort
        graph
  in
    foldr
      ( \(Tuple key value) body ->
          let
            name = VarName $ show key

            inputCount = inputs value

            finalValue =
              if inputCount == 0 then
                value
              else
                FixPoint Nowhere $ Lambda Nowhere name value
          in
            Let Nowhere name finalValue body
      )
      (Variable Nowhere $ VarName $ show main)
      $ catMaybes
      $ compileGraphNode toExpression graph
      <$> sorted
