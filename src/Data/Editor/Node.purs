module Lunarbox.Data.Editor.Node
  ( Node(..)
  , ComplexNodeData
  , compileNode
  , hasOutput
  , getInputs
  , _ComplexNodeFunction
  , _ComplexNodeInputs
  , _OutputNode
  , _nodeInputs
  ) where

import Prelude
import Data.Lens (Lens', Prism', Traversal', is, lens, prism', set)
import Data.Lens.Record (prop)
import Data.List (List(..), foldl, mapWithIndex, (!!))
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Lunarbox.Data.Dataflow.Expression (Expression(..), VarName(..), wrap)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..), nothing)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Editor.Node.PinLocation (NodeOrPinLocation, Pin(..), inputNode, outputNode)
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

instance showNode :: Show Node where
  show InputNode = "InputNode"
  show (OutputNode id) = "Output " <> maybe "???" show id
  show (ComplexNode data') = show data'

-- Check if a node has an output pin
hasOutput :: Node -> Boolean
hasOutput = not <<< is _OutputNode

-- Get all inputs of a node
getInputs :: Node -> List (Maybe NodeId)
getInputs = case _ of
  ComplexNode { inputs } -> inputs
  OutputNode input -> pure input
  InputNode -> Nil

functionCall :: forall l l'. ExtendedLocation l l' -> Expression (ExtendedLocation l l') -> List (Expression (ExtendedLocation l l')) -> Expression (ExtendedLocation l l')
functionCall location calee = wrap location <<< foldl (FunctionCall Nowhere) calee

compileNode :: G.Graph NodeId Node -> NodeId -> Expression NodeOrPinLocation -> Expression NodeOrPinLocation
compileNode nodes id child =
  flip (maybe nothing) (G.lookup id nodes) case _ of
    InputNode -> inputNode id child
    OutputNode outputId ->
      outputNode id case outputId of
        Just outputId' -> Variable (Location outputId') $ VarName $ show outputId'
        Nothing -> nothing
    ComplexNode { inputs, function } -> Let Nowhere name value child
      where
      name = VarName $ show id

      calee = Variable (Location id) $ VarName $ show function

      arguments =
        mapWithIndex
          ( \index id' ->
              wrap (DeepLocation id $ InputPin index) case id' of
                Just id'' -> Variable Nowhere $ VarName $ show id''
                Nothing -> nothing
          )
          inputs

      value = functionCall (DeepLocation id OutputPin) calee arguments

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
