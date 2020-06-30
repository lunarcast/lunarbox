module Lunarbox.Data.Editor.Project
  ( Project(..)
  , compileProject
  , createEmptyFunction
  , emptyProject
  , createFunction
  , getFunctions
  , _atProjectFunction
  , _atProjectNode
  , _ProjectFunctions
  , _ProjectMain
  , _projectNodeGroup
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, encodeJson, jsonEmptyObject, (:=), (~>))
import Data.Filterable (filter)
import Data.Foldable (foldr)
import Data.Lens (Lens', Traversal', _Just, is, set, view)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)
import Lunarbox.Data.Class.GraphRep (class GraphRep, toGraph)
import Lunarbox.Data.Dataflow.Expression (Expression(..), VarName(..), wrap)
import Lunarbox.Data.Editor.DataflowFunction (DataflowFunction(..), _VisualFunction, compileDataflowFunction)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.Location (Location(..))
import Lunarbox.Data.Editor.Node (Node(..))
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Editor.NodeGroup (NodeGroup(..), _NodeGroupNodes)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.Lens (newtypeIso)

newtype Project
  = Project
  { functions :: Map.Map FunctionName DataflowFunction
  , main :: FunctionName
  }

derive instance newtypeProject :: Newtype Project _

instance encodeJsonProject :: EncodeJson Project where
  encodeJson (Project obj) =
    "main" := encodeJson obj.main ~> "functions"
      := encodeJson functions'
      ~> jsonEmptyObject
    where
    functions' = filter (is _VisualFunction) obj.functions

derive newtype instance decodeJsonProject :: DecodeJson Project

instance graphRepProject :: GraphRep Project FunctionName DataflowFunction where
  toGraph (Project { functions }) = toGraph functions

_ProjectFunctions :: Lens' Project (Map.Map FunctionName DataflowFunction)
_ProjectFunctions = newtypeIso <<< prop (SProxy :: _ "functions")

_ProjectMain :: Lens' Project FunctionName
_ProjectMain = newtypeIso <<< prop (SProxy :: _ "main")

-- -- Takes a key and a graph and uses that to produce an Expression
-- compileGraphNode :: forall k v l. Ord k => (v -> Expression l) -> Graph k v -> k -> Maybe (Tuple k (Expression (ExtendedLocation k l)))
-- compileGraphNode toExpression graph key = Tuple key <$> wrap (Location key) <$> map (DeepLocation key) <$> toExpression <$> view (at key) graph
-- | Compile a visual program into a linear expression
compileProject :: Project -> Expression Location
compileProject project@(Project { main }) =
  foldr
    ( \(Tuple key value) body ->
        let
          name = VarName $ show key
        in
          Let UnknownLocation name value body
    )
    (Variable UnknownLocation $ VarName $ show main)
    $ List.catMaybes
    $ compileFunction
    <$> sorted
  where
  compileFunction name = Tuple name <$> wrap (AtFunction name) <$> map (InsideFunction name) <$> compileDataflowFunction <$> G.lookup name graph

  graph = toGraph project

  sorted =
    G.topologicalSort
      graph

createEmptyFunction :: NodeId -> DataflowFunction
createEmptyFunction id =
  VisualFunction
    $ NodeGroup
        { inputs: mempty
        , nodes: Map.singleton id $ OutputNode Nothing
        , output: id
        }

emptyProject :: NodeId -> Project
emptyProject id =
  Project
    { main: FunctionName "main"
    , functions: Map.singleton (FunctionName "main") function
    }
  where
  function = createEmptyFunction id

createFunction :: FunctionName -> NodeId -> Project -> Project
createFunction name outputId =
  set
    (_atProjectFunction name)
    $ Just
    $ createEmptyFunction outputId

getFunctions :: forall u. Unfoldable u => Project -> u FunctionName
getFunctions = Set.toUnfoldable <<< Map.keys <<< view _ProjectFunctions

_atProjectFunction :: FunctionName -> Traversal' Project (Maybe DataflowFunction)
_atProjectFunction name = _ProjectFunctions <<< at name

_projectNodeGroup :: FunctionName -> Traversal' Project NodeGroup
_projectNodeGroup name = _ProjectFunctions <<< at name <<< _Just <<< _VisualFunction

_atProjectNodeGroup :: FunctionName -> Traversal' Project NodeGroup
_atProjectNodeGroup name = _ProjectFunctions <<< at name <<< _Just <<< _VisualFunction

_atProjectNode :: FunctionName -> NodeId -> Traversal' Project (Maybe Node)
_atProjectNode name id = _atProjectNodeGroup name <<< _NodeGroupNodes <<< at id
