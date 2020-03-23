module Lunarbox.Data.Project where

import Prelude
import Data.Graph (Graph, insertVertex, lookup, topologicalSort, vertices) as G
import Data.Lens (Lens', Prism', prism')
import Data.Lens.Record (prop)
import Data.List (List, foldl, reverse, (\\))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Data.Unfoldable (class Unfoldable)
import Lunarbox.Data.Graph (singleton, keys) as G
import Lunarbox.Data.Lens (newtypeIso)
import Lunarbox.Dataflow.Expressible (class Expressible, newtypeToExpression, toExpression)
import Lunarbox.Dataflow.Expression (Expression(..), NativeExpression)
import Lunarbox.Dataflow.Type (TVar(..))

newtype NodeId
  = NodeId String

derive instance eqNodeId :: Eq NodeId

derive instance ordNodeId :: Ord NodeId

derive instance newtypeNodeId :: Newtype NodeId _

instance showNodeId :: Show NodeId where
  show = show <<< unwrap

instance expressibleNodeId :: Expressible NodeId where
  toExpression = newtypeToExpression

_NodeId :: Lens' NodeId String
_NodeId = newtypeIso

newtype FunctionName
  = FunctionName String

derive instance eqFunctionName :: Eq FunctionName

derive instance ordFunctionName :: Ord FunctionName

derive instance newtypeFunctionName :: Newtype FunctionName _

instance showFunctionName :: Show FunctionName where
  show (FunctionName f) = f

instance expressibleFunctionName :: Expressible FunctionName where
  toExpression = newtypeToExpression

_FunctionName :: Lens' FunctionName String
_FunctionName = newtypeIso

type ComplexNodeData
  = { inputs :: List (Maybe NodeId)
    , function :: FunctionName
    }

data Node
  = InputNode
  | ComplexNode
    ComplexNodeData
  | OutputNode (Maybe NodeId)

_InputNode :: Prism' Node ComplexNodeData
_InputNode =
  prism' ComplexNode case _ of
    ComplexNode c -> Just c
    _ -> Nothing

_OutputNode :: Prism' Node (Maybe NodeId)
_OutputNode =
  prism' OutputNode case _ of
    OutputNode v -> Just v
    _ -> Nothing

type NodeGroupData a
  = { inputs :: List NodeId
    , nodes :: G.Graph NodeId (Tuple Node a)
    , output :: NodeId
    }

newtype NodeGroup a
  = NodeGroup (NodeGroupData a)

derive instance newtypeNodeGroup :: Newtype (NodeGroup a) _

_NodeGroup :: forall a. Lens' (NodeGroup a) (NodeGroupData a)
_NodeGroup = newtypeIso

_inputs :: forall a. Lens' (NodeGroupData a) (List NodeId)
_inputs = prop (SProxy :: _ "inputs")

_nodes :: forall a. Lens' (NodeGroupData a) (G.Graph NodeId (Tuple Node a))
_nodes = prop (SProxy :: _ "nodes")

_output :: forall a. Lens' (NodeGroupData a) NodeId
_output = prop (SProxy :: _ "output")

data VisualFunction a
  = NativeVF (forall a b. NativeExpression a b)
  | DataflowFunction (NodeGroup a)

_DataflowFunction :: forall a. Prism' (VisualFunction a) (NodeGroup a)
_DataflowFunction =
  prism' DataflowFunction case _ of
    DataflowFunction f -> Just f
    _ -> Nothing

type Project a
  = { functions :: G.Graph FunctionName (VisualFunction a)
    , main :: FunctionName
    }

_functions :: forall a. Lens' (Project a) (G.Graph FunctionName (VisualFunction a))
_functions = prop (SProxy :: _ "functions")

_main :: forall a. Lens' (Project a) FunctionName
_main = prop (SProxy :: _ "main")

orderNodes :: forall a. NodeGroup a -> List NodeId
orderNodes (NodeGroup function) = G.topologicalSort function.nodes

functionDeclaration :: Expression -> List TVar -> Expression
functionDeclaration expr = foldl (flip Lambda) expr <<< reverse

functionCall :: Expression -> List Expression -> Expression
functionCall = foldl FunctionCall

compileNode :: G.Graph NodeId Node -> Maybe Expression -> NodeId -> Maybe Expression
compileNode nodes maybeChild id =
  G.lookup id nodes
    >>= case _ of
        InputNode -> maybeChild
        OutputNode childId -> pure $ toExpression $ childId
        ComplexNode { inputs, function: functionName } ->
          pure
            $ case maybeChild of
                Just child -> Let (TV $ show $ id) value child
                Nothing -> value
          where
          value = functionCall (toExpression $ functionName) (toExpression <$> inputs)

instance expressibleNodeGroup :: Expressible (NodeGroup a) where
  toExpression group@(NodeGroup { nodes, inputs }) =
    toExpression do
      let
        ordered = orderNodes group

        body = ordered \\ inputs
      return <-
        foldl
          (compileNode $ fst <$> nodes)
          Nothing
          body
      pure $ functionDeclaration return $ TV <$> unwrap <$> inputs

instance expressibleVisualFunction :: Expressible (VisualFunction a) where
  toExpression = case _ of
    NativeVF f -> Native f
    DataflowFunction g -> toExpression g

compileProject :: forall a. Project a -> List Expression
compileProject project = toExpression <$> G.vertices project.functions

createEmptyFunction :: forall a. a -> NodeId -> VisualFunction a
createEmptyFunction data' id =
  DataflowFunction
    $ NodeGroup
        { inputs: mempty
        , nodes: G.singleton id $ Tuple (OutputNode Nothing) data'
        , output: id
        }

emptyProject :: forall a. a -> NodeId -> Project a
emptyProject data' id =
  { main: FunctionName "main"
  , functions: G.singleton (FunctionName "main") $ createEmptyFunction data' id
  }

createFunction :: forall a. a -> FunctionName -> NodeId -> Project a -> Project a
createFunction data' name outputId project@{ functions } =
  project
    { functions = G.insertVertex name function functions }
  where
  function = (createEmptyFunction data' outputId)

getFunctions :: forall u a. Unfoldable u => Project a -> u FunctionName
getFunctions project = project.functions # G.keys
