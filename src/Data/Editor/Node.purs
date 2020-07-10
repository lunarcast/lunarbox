module Lunarbox.Data.Editor.Node
  ( Node(..)
  , ComplexNodeData
  , compileNode
  , hasOutput
  , getInputs
  , getFunctionName
  , connectedInputs
  , _nodeInput
  , _ComplexNodeFunction
  , _ComplexNodeInputs
  , _OutputNode
  , _ComplexNode
  , _nodeInputs
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Compactable (compact)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', Prism', Traversal', is, lens, prism', set)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.List (List(..), (:), mapWithIndex, (!!))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), uncurry)
import Lunarbox.Data.Dataflow.Expression (Expression(..), VarName(..), wrap)
import Lunarbox.Data.Editor.Class.Depends (class Depends)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..), ScopedLocation(..), inputNode, outputNode)
import Lunarbox.Data.Functor (indexed)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.Lens (listToArrayIso)

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

derive instance eqNode :: Eq Node

derive instance genericNode :: Generic Node _

instance encodeJsonNode :: EncodeJson Node where
  encodeJson = genericEncodeJson

instance decodeJsonNode :: DecodeJson Node where
  decodeJson = genericDecodeJson

instance showNode :: Show Node where
  show InputNode = "InputNode"
  show (OutputNode id) = "Output " <> maybe "???" show id
  show (ComplexNode data') = show data'

instance dependsNode :: Depends Node NodeId where
  getDependencies (OutputNode (Just id)) = Set.singleton id
  getDependencies (ComplexNode { inputs }) = Set.fromFoldable $ compact inputs
  getDependencies _ = mempty

-- Check if a node has an output pin
hasOutput :: Node -> Boolean
hasOutput = not <<< is _OutputNode

-- Get the name of the function the node runs
getFunctionName :: Node -> FunctionName
getFunctionName = case _ of
  ComplexNode { function } -> function
  InputNode -> FunctionName "input"
  OutputNode _ -> FunctionName "output"

-- Get all inputs of a node
getInputs :: Node -> List (Maybe NodeId)
getInputs = case _ of
  ComplexNode { inputs } -> inputs
  OutputNode input -> pure input
  InputNode -> Nil

-- Get all the pairs (index, id) of connected inputs
connectedInputs :: Node -> List (Tuple Int NodeId)
connectedInputs = List.catMaybes <<< map (uncurry $ (<$>) <<< Tuple) <<< indexed <<< getInputs

-- Declare a call on a curried function with any number of arguments
functionCall :: ScopedLocation -> Expression ScopedLocation -> NodeId -> List (Expression ScopedLocation) -> Expression ScopedLocation
functionCall location calee id = wrap location <<< foldlWithIndex (FunctionCall <<< AtApplication id) calee

-- Compile a node into an expression
compileNode :: G.Graph NodeId Node -> NodeId -> Expression ScopedLocation -> Expression ScopedLocation
compileNode nodes id child =
  flip (maybe $ TypedHole $ UnexistingNode id) (G.lookup id nodes) case _ of
    InputNode -> inputNode id child
    OutputNode outputId ->
      outputNode id case outputId of
        Just outputId' -> Variable location $ VarName $ show outputId'
        Nothing -> TypedHole location
      where
      location = PinLocation id $ InputPin 1
    ComplexNode
      { inputs: cond : then' : else' : Nil
    , function: FunctionName "if"
    } -> Let (NodeDefinition id) name value child
      where
      name = VarName $ show id

      value = If (NodeDefinition id) condExpr thenExpr elseExpr

      condExpr = mkExpr 0 cond

      thenExpr = mkExpr 1 then'

      elseExpr = mkExpr 2 else'

      mkExpr = makePinExpression id
    ComplexNode { inputs, function } -> Let (NodeDefinition id) name value child
      where
      name = VarName $ show id

      calee = Variable (FunctionUsage function) $ VarName $ show function

      arguments =
        mapWithIndex
          (makePinExpression id)
          inputs

      value = wrap (NodeLocation id) $ functionCall (PinLocation id OutputPin) calee id arguments

-- | Helper to create an expression from some data aboout a pin
makePinExpression :: NodeId -> Int -> (Maybe NodeId) -> Expression ScopedLocation
makePinExpression node index = case _ of
  Just id' -> Variable location $ VarName $ show id'
  Nothing -> TypedHole location
  where
  location = PinLocation node $ InputPin index

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

_OutputNode :: Prism' Node (Maybe NodeId)
_OutputNode =
  prism' OutputNode case _ of
    OutputNode v -> Just v
    _ -> Nothing

_nodeInputs :: Lens' Node (List (Maybe NodeId))
_nodeInputs =
  lens getInputs \node -> case node of
    InputNode -> const node
    OutputNode inner -> OutputNode <<< join <<< (_ !! 0)
    ComplexNode _ -> flip (set _ComplexNodeInputs) node

-- Lens for an individual node id
_nodeInput :: Int -> Traversal' Node (Maybe NodeId)
_nodeInput index = _nodeInputs <<< listToArrayIso <<< ix index
