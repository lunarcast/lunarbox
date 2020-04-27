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
import Data.Lens (Lens', Traversal', _Just, set, view)
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Unfoldable (class Unfoldable)
import Lunarbox.Data.Dataflow.Expression (Expression, optimize)
import Lunarbox.Data.Dataflow.Graph (compileGraph)
import Lunarbox.Data.Editor.DataflowFunction (DataflowFunction(..), _VisualFunction, compileDataflowFunction)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.Location (Location)
import Lunarbox.Data.Editor.Node (Node(..))
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Editor.NodeGroup (NodeGroup(..), _NodeGroupNodes)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.Lens (newtypeIso)

newtype Project
  = Project
  { functions :: G.Graph FunctionName DataflowFunction
  , main :: FunctionName
  }

derive instance newtypeProject :: Newtype Project _

_ProjectFunctions :: Lens' Project (G.Graph FunctionName DataflowFunction)
_ProjectFunctions = newtypeIso <<< prop (SProxy :: _ "functions")

_ProjectMain :: Lens' Project FunctionName
_ProjectMain = newtypeIso <<< prop (SProxy :: _ "main")

compileProject :: Project -> Expression Location
compileProject (Project { functions, main }) = optimize $ compileGraph compileDataflowFunction functions main

createEmptyFunction :: NodeId -> DataflowFunction
createEmptyFunction id =
  VisualFunction
    $ NodeGroup
        { inputs: mempty
        , nodes: G.singleton id $ OutputNode Nothing
        , output: id
        }

emptyProject :: NodeId -> Project
emptyProject id =
  Project
    { main: FunctionName "main"
    , functions: G.singleton (FunctionName "main") function
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
getFunctions = Set.toUnfoldable <<< G.keys <<< view _ProjectFunctions

_atProjectFunction :: FunctionName -> Traversal' Project (Maybe DataflowFunction)
_atProjectFunction name = _ProjectFunctions <<< at name

_projectNodeGroup :: FunctionName -> Traversal' Project NodeGroup
_projectNodeGroup name = _ProjectFunctions <<< at name <<< _Just <<< _VisualFunction

_atProjectNodeGroup :: FunctionName -> Traversal' Project NodeGroup
_atProjectNodeGroup name = _ProjectFunctions <<< at name <<< _Just <<< _VisualFunction

_atProjectNode :: FunctionName -> NodeId -> Traversal' Project (Maybe Node)
_atProjectNode name id = _atProjectNodeGroup name <<< _NodeGroupNodes <<< at id
