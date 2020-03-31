module Lunarbox.Data.Project where

import Prelude
import Data.Lens (Lens', Prism', Traversal', _1, _2, over, prism')
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

type Project f n
  = { functions :: G.Graph FunctionName (Tuple (DataflowFunction n) f)
    , main :: FunctionName
    }

_functions :: forall f n. Lens' (Project f n) (G.Graph FunctionName (Tuple (DataflowFunction n) f))
_functions = prop (SProxy :: _ "functions")

_main :: forall f n. Lens' (Project f n) FunctionName
_main = prop (SProxy :: _ "main")

instance expressibleVisualFunction :: Expressible (DataflowFunction a) where
  toExpression = case _ of
    NativeFunction f -> Native f
    VisualFunction g -> toExpression g

compileProject :: forall f n. Project f n -> List Expression
compileProject project = toExpression <$> fst <$> G.vertices project.functions

createEmptyFunction :: forall a. a -> NodeId -> DataflowFunction a
createEmptyFunction data' id =
  VisualFunction
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

_atProjectFunction :: forall f n. FunctionName -> Traversal' (Project f n) (Maybe (Tuple (DataflowFunction n) f))
_atProjectFunction name = _functions <<< at name

_projectNodeGroup :: forall f n. FunctionName -> Traversal' (Project f n) (NodeGroup n)
_projectNodeGroup name = _functions <<< ix name <<< _1 <<< _VisualFunction

_atProjectNode :: forall f n. FunctionName -> NodeId -> Traversal' (Project f n) (Maybe (Tuple Node n))
_atProjectNode name id = _projectNodeGroup name <<< _NodeGroupNodes <<< at id

_projectFunctionData :: forall f n. FunctionName -> Traversal' (Project f n) f
_projectFunctionData name = _functions <<< ix name <<< _2
