module Lunarbox.Data.Editor.Node
  ( Node(..)
  , ComplexNodeData
  , compileNode
  , _ComplexNodeFunction
  , _ComplexNodeInputs
  , _OutputNode
  ) where

import Prelude
import Data.Lens (Prism', Traversal', prism')
import Data.Lens.Record (prop)
import Data.List (List, foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Lunarbox.Data.Dataflow.Class.Expressible (nullExpr, toExpressionFromSelf, toExpressionWithLocation)
import Lunarbox.Data.Dataflow.Expression (Expression(..), VarName(..))
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Node.NodeId (NodeId(..))
import Lunarbox.Data.Graph as G

type ComplexNodeData
  = { inputs :: List (Maybe NodeId)
    , function :: FunctionName
    }

-- Curently we have 3 types of nodes:
-- 1) Inputs nodes: (point to nothing)
-- 2) Complex nodes: points to multiple nodes and call a function
-- 3) Output nodes: only points to 1 value which it returns
data Node
  = InputNode
  | ComplexNode
    ComplexNodeData
  | OutputNode (Maybe NodeId)

functionCall :: forall l. l -> Expression l -> List (Expression l) -> Expression l
functionCall = foldl <<< FunctionCall

compileNode :: G.Graph NodeId Node -> NodeId -> Maybe (Expression NodeId) -> Maybe (Expression NodeId)
compileNode nodes id maybeChild =
  G.lookup id nodes
    >>= case _ of
        InputNode -> maybeChild
        OutputNode childId -> Just $ maybe (nullExpr id) (toExpressionWithLocation id) childId
        ComplexNode { inputs, function: functionName } ->
          pure
            $ case maybeChild of
                Just child -> Let id (VarName $ show id) value child
                Nothing -> value
          where
          value =
            functionCall id (toExpressionFromSelf $ NodeId $ show functionName)
              ( maybe (nullExpr id)
                  toExpressionFromSelf
                  <$> inputs
              )

-- Lenses
_ComplexNode :: Prism' Node ComplexNodeData
_ComplexNode =
  prism' ComplexNode case _ of
    ComplexNode c -> Just c
    _ -> Nothing

_ComplexNodeInputs :: Traversal' Node (List (Maybe NodeId))
_ComplexNodeInputs = _ComplexNode <<< prop (SProxy :: _ "inputs")

_ComplexNodeFunction :: Traversal' Node FunctionName
_ComplexNodeFunction = _ComplexNode <<< prop (SProxy :: _ "function")

_OutputNode :: Prism' Node NodeId
_OutputNode =
  prism' (OutputNode <<< Just) case _ of
    OutputNode v -> v
    _ -> Nothing
