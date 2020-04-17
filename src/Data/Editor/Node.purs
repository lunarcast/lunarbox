module Lunarbox.Data.Editor.Node
  ( Node(..)
  , ComplexNodeData
  , compileNode
  , hasOutput
  , _ComplexNodeFunction
  , _ComplexNodeInputs
  , _OutputNode
  ) where

import Prelude
import Data.Default (def)
import Data.Lens (Prism', Traversal', is, prism')
import Data.Lens.Record (prop)
import Data.List (List, foldl, mapWithIndex)
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

-- Check if a node has an output pin
hasOutput :: Node -> Boolean
hasOutput = not <<< is _OutputNode

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
    ComplexNode { inputs, function } -> Let def name value child
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
