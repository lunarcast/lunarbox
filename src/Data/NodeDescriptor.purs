module Lunarbox.Data.NodeDescriptor where

import Prelude
import Data.Array as Array
import Data.Lens (is, view)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Lunarbox.Data.Dataflow.FunctionName (FunctionName)
import Lunarbox.Data.FunctionData (FunctionData, _FunctionDataExternal)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.NodeData (NodeData)
import Lunarbox.Data.Project (DataflowFunction, Project, _VisualFunction)

type NodeDescriptor
  = { isUsable :: Boolean
    , isEditable :: Boolean
    }

type FunctionGraphNode
  = { name :: FunctionName
    , functionData :: FunctionData
    , function :: DataflowFunction NodeData
    }

describe :: Maybe FunctionName -> Project FunctionData NodeData -> Array (Tuple FunctionGraphNode NodeDescriptor)
describe currentFunction project =
  G.toUnfoldable project.functions
    <#> \(Tuple name (Tuple function functionData)) ->
        let
          isExternal = view _FunctionDataExternal functionData

          isEditable = not isExternal && is _VisualFunction function

          isUsable = currentFunction /= Just name && isJust currentFunction
        in
          Tuple { name, function, functionData } { isUsable, isEditable }

onlyEditable :: Maybe FunctionName -> Project FunctionData NodeData -> Array (Tuple FunctionGraphNode NodeDescriptor)
onlyEditable c p = Array.filter (\(Tuple _ { isEditable }) -> isEditable) $ describe c p
