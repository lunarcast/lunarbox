module Lunarbox.Data.Editor.NodeGroup
  ( NodeGroup(..)
  , orderNodes
  , _NodeGroupInputs
  , _NodeGroupOutput
  , _NodeGroupNodes
  ) where

import Prelude
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.List (List, foldl, (\\))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple, fst)
import Lunarbox.Data.Dataflow.Class.Expressible (class Expressible, nullExpr)
import Lunarbox.Data.Dataflow.Expression (VarName(..), functionDeclaration)
import Lunarbox.Data.Editor.Node (Node, compileNode)
import Lunarbox.Data.Editor.Node.NodeId (NodeId(..))
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

instance expressibleNodeGroup :: Expressible (NodeGroup a) NodeId where
  toExpression group@(NodeGroup { nodes, inputs }) =
    let
      ordered = orderNodes group

      body = ordered \\ inputs

      return =
        fromMaybe (nullExpr $ NodeId "unconnected")
          $ foldl
              (flip $ compileNode $ fst <$> nodes)
              Nothing
              body
    in
      functionDeclaration (NodeId "this should not be seen") return $ VarName <$> unwrap <$> inputs

-- Prism
_NodeGroupInputs :: forall a. Lens' (NodeGroup a) (List NodeId)
_NodeGroupInputs = newtypeIso <<< prop (SProxy :: _ "inputs")

_NodeGroupNodes :: forall a. Lens' (NodeGroup a) (Graph NodeId (Tuple Node a))
_NodeGroupNodes = newtypeIso <<< prop (SProxy :: _ "nodes")

_NodeGroupOutput :: forall a. Lens' (NodeGroup a) NodeId
_NodeGroupOutput = newtypeIso <<< prop (SProxy :: _ "output")
