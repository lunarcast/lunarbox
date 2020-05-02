module Lunarbox.Data.Editor.Node.NodeDescriptor
  ( describe
  , onlyEditable
  , NodeDescriptor
  ) where

import Prelude
import Data.Lens (is)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Lunarbox.Data.Editor.DataflowFunction (_VisualFunction)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Project (Project(..))
import Lunarbox.Data.Graph as G

type NodeDescriptor
  = { isUsable :: Boolean
    , isEditable :: Boolean
    }

describe :: Maybe FunctionName -> Project -> Map FunctionName NodeDescriptor
describe currentFunction (Project { functions }) =
  flip (Map.mapMaybeWithKey) (G.toMap functions) \name function ->
    let
      isCurrent = currentFunction == Just name

      -- TODO: make this actually check the NodeData
      isExternal = false

      isEditable =
        not isCurrent
          && not isExternal
          && is _VisualFunction function

      isUsable = isJust currentFunction
    in
      Just { isUsable, isEditable }

onlyEditable :: Maybe FunctionName -> Project -> Map FunctionName NodeDescriptor
onlyEditable c p = Map.filter (_.isEditable) $ describe c p
