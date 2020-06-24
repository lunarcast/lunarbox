module Lunarbox.Data.Editor.NodeGroup
  ( NodeGroup(..)
  , compileNodeGroup
  , _NodeGroupInputs
  , _NodeGroupOutput
  , _NodeGroupNodes
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Lens (Lens', view)
import Data.Lens.Record (prop)
import Data.List (List, foldMap, foldr, (:), (\\))
import Data.Map (Map)
import Data.Newtype (class Newtype, unwrap)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Lunarbox.Data.Class.GraphRep (toGraph)
import Lunarbox.Data.Dataflow.Expression (Expression, VarName(..), functionDeclaration)
import Lunarbox.Data.Editor.Class.Depends (class Depends)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..), nothing)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Node (Node(..), compileNode)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Editor.Node.PinLocation (NodeOrPinLocation)
import Lunarbox.Data.Graph (topologicalSort)
import Lunarbox.Data.Lens (newtypeIso)

-- Represents a graph of nodes
newtype NodeGroup
  = NodeGroup
  { inputs :: List NodeId
  , nodes :: Map NodeId Node
  , output :: NodeId
  }

derive instance newtypeNodeGroup :: Newtype NodeGroup _

derive newtype instance showNodeGroup :: Show NodeGroup

derive newtype instance encodeJsonNodeGroup :: EncodeJson NodeGroup

derive newtype instance decodeJsonNodeGroup :: DecodeJson NodeGroup

instance dependencyNodeGroup :: Depends NodeGroup FunctionName where
  getDependencies =
    view _NodeGroupNodes
      >>> foldMap case _ of
          -- Only complex nodes reference other functions so those are the only ones we take into consideration
          ComplexNode { function } -> Set.singleton function
          _ -> mempty

compileNodeGroup :: NodeGroup -> Expression NodeOrPinLocation
compileNodeGroup group@(NodeGroup { nodes, output, inputs }) =
  let
    graph = toGraph nodes

    ordered = topologicalSort graph

    bodyNodes = (ordered \\ (output : inputs)) <> pure output

    return =
      foldr
        (compileNode graph)
        nothing
        bodyNodes
  in
    functionDeclaration Nowhere return $ VarName <$> unwrap <$> inputs

-- Prism
_NodeGroupInputs :: Lens' NodeGroup (List NodeId)
_NodeGroupInputs = newtypeIso <<< prop (SProxy :: _ "inputs")

_NodeGroupNodes :: Lens' NodeGroup (Map NodeId Node)
_NodeGroupNodes = newtypeIso <<< prop (SProxy :: _ "nodes")

_NodeGroupOutput :: Lens' NodeGroup NodeId
_NodeGroupOutput = newtypeIso <<< prop (SProxy :: _ "output")
