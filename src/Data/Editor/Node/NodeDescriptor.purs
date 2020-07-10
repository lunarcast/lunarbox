module Lunarbox.Data.Editor.Node.NodeDescriptor
  ( describe
  , onlyEditable
  , NodeDescriptor
  ) where

import Prelude
import Data.Lens (is)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Lunarbox.Data.Class.GraphRep (toGraph)
import Lunarbox.Data.Editor.DataflowFunction (_VisualFunction)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Project (Project(..))
import Lunarbox.Data.Graph as G

type NodeDescriptor
  = { isUsable :: Boolean
    , isEditable :: Boolean
    , canBeDeleted :: Boolean
    }

describe :: FunctionName -> Project -> Map FunctionName NodeDescriptor
describe currentFunction project@(Project { functions, main }) =
  flip (Map.mapMaybeWithKey) functions \name function ->
    let
      isCurrent = currentFunction == name

      -- TODO: make this actually check the NodeData
      isExternal = false

      isVisual = is _VisualFunction function

      isEditable =
        not isCurrent
          && not isExternal
          && isVisual

      wouldCycle = G.wouldCreateLongCycle name currentFunction $ toGraph project

      isUsable = isCurrent || not wouldCycle

      canBeDeleted = isVisual && main /= name
    in
      Just { isUsable, isEditable, canBeDeleted }

onlyEditable :: FunctionName -> Project -> Map FunctionName NodeDescriptor
onlyEditable c p = Map.filter _.isEditable $ describe c p
