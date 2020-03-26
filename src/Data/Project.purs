module Lunarbox.Data.Project where

import Prelude
import Data.Lens (Lens', Prism', Traversal', _1, over, prism')
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.List (List, foldl, reverse, (\\))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Data.Unfoldable (class Unfoldable)
import Lunarbox.Data.Dataflow.FunctionName (FunctionName(..))
import Lunarbox.Data.Dataflow.NodeId (NodeId)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.Lens (newtypeIso)
import Lunarbox.Dataflow.Expressible (class Expressible, toExpression)
import Lunarbox.Dataflow.Expression (Expression(..), NativeExpression)
import Lunarbox.Dataflow.Type (TVar(..))

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

_NodeGroupInputs :: forall a. Lens' (NodeGroup a) (List NodeId)
_NodeGroupInputs = _NodeGroup <<< prop (SProxy :: _ "inputs")

_NodeGroupNodes :: forall a. Lens' (NodeGroup a) (G.Graph NodeId (Tuple Node a))
_NodeGroupNodes = _NodeGroup <<< prop (SProxy :: _ "nodes")

_NodeGroupOutput :: forall a. Lens' (NodeGroup a) NodeId
_NodeGroupOutput = _NodeGroup <<< prop (SProxy :: _ "output")

data VisualFunction a
  = NativeVF (forall a b. NativeExpression a b)
  | DataflowFunction (NodeGroup a)

_DataflowFunction :: forall a. Prism' (VisualFunction a) (NodeGroup a)
_DataflowFunction =
  prism' DataflowFunction case _ of
    DataflowFunction f -> Just f
    _ -> Nothing

type Project f n
  = { functions :: G.Graph FunctionName (Tuple (VisualFunction n) f)
    , main :: FunctionName
    }

_functions :: forall f n. Lens' (Project f n) (G.Graph FunctionName (Tuple (VisualFunction n) f))
_functions = prop (SProxy :: _ "functions")

_main :: forall f n. Lens' (Project f n) FunctionName
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

compileProject :: forall f n. Project f n -> List Expression
compileProject project = toExpression <$> fst <$> G.vertices project.functions

createEmptyFunction :: forall a. a -> NodeId -> VisualFunction a
createEmptyFunction data' id =
  DataflowFunction
    $ NodeGroup
        { inputs: mempty
        , nodes: G.singleton id $ Tuple (OutputNode Nothing) data'
        , output: id
        }

emptyProject :: forall f n. f -> n -> NodeId -> Project f n
emptyProject functionData nodeData id =
  { main: FunctionName "main"
  , functions: G.singleton (FunctionName "main") $ Tuple function functionData
  }
  where
  function = createEmptyFunction nodeData id

createFunction :: forall f n. f -> n -> FunctionName -> NodeId -> Project f n -> Project f n
createFunction functionData nodeData name outputId =
  over
    _functions
    $ G.insert name (Tuple function functionData)
  where
  function = createEmptyFunction nodeData outputId

getFunctions :: forall u a b. Unfoldable u => Project a b -> u FunctionName
getFunctions project = project.functions # G.keys # Set.toUnfoldable

_projectNodeGroup :: forall f n. FunctionName -> Traversal' (Project f n) (NodeGroup n)
_projectNodeGroup name = _functions <<< ix name <<< _1 <<< _DataflowFunction

_atProjectNode :: forall f n. FunctionName -> NodeId -> Traversal' (Project f n) (Maybe (Tuple Node n))
_atProjectNode name id = _projectNodeGroup name <<< _NodeGroupNodes <<< at id
