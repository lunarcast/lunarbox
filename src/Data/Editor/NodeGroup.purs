module Lunarbox.Data.Editor.NodeGroup
  ( NodeGroup(..)
  , orderNodes
  , compileNodeGroup
  , _NodeGroupInputs
  , _NodeGroupOutput
  , _NodeGroupNodes
  ) where

import Prelude
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.List (List, foldl, (\\), (:))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple, fst)
import Lunarbox.Data.Dataflow.Expression (Expression, VarName(..), functionDeclaration)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..), nothing)
import Lunarbox.Data.Editor.Node (Node, compileNode)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Editor.Node.PinLocation (NodeOrPinLocation)
import Lunarbox.Data.Graph (Graph, topologicalSort)
import Lunarbox.Data.Lens (newtypeIso)

-- Represents a graph of nodes
newtype NodeGroup a
  = NodeGroup
  { inputs :: List NodeId
  , nodes :: Graph NodeId (Tuple Node a)
  , output :: NodeId
  }

-- Take a graph of nodes and return a list of nodes sorted in topological order
orderNodes :: forall a. NodeGroup a -> List NodeId
orderNodes (NodeGroup function) = topologicalSort function.nodes

derive instance newtypeNodeGroup :: Newtype (NodeGroup a) _

compileNodeGroup :: forall a. NodeGroup a -> Expression NodeOrPinLocation
compileNodeGroup group@(NodeGroup { nodes, output, inputs }) =
  let
    ordered = orderNodes group

    nodeGraph = fst <$> nodes

    bodyNodes = output : (ordered \\ inputs)

    return =
      foldl
        (flip $ compileNode nodeGraph)
        nothing
        bodyNodes
  in
    functionDeclaration Nowhere return $ VarName <$> unwrap <$> inputs

-- Prism
_NodeGroupInputs :: forall a. Lens' (NodeGroup a) (List NodeId)
_NodeGroupInputs = newtypeIso <<< prop (SProxy :: _ "inputs")

_NodeGroupNodes :: forall a. Lens' (NodeGroup a) (Graph NodeId (Tuple Node a))
_NodeGroupNodes = newtypeIso <<< prop (SProxy :: _ "nodes")

_NodeGroupOutput :: forall a. Lens' (NodeGroup a) NodeId
_NodeGroupOutput = newtypeIso <<< prop (SProxy :: _ "output")
