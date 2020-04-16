module Lunarbox.Data.Editor.Node.NodeDescriptor
  ( describe
  , onlyEditable
  , FunctionGraphNode
  , NodeDescriptor
  ) where

import Prelude
import Data.Array as Array
import Data.Lens (is, view)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Lunarbox.Data.Graph as G
import Lunarbox.Data.Editor.DataflowFunction (DataflowFunction, _VisualFunction)
import Lunarbox.Data.Editor.FunctionData (FunctionData, _FunctionDataExternal)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Node.NodeData (NodeData)
import Lunarbox.Data.Editor.Project (Project(..))

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
describe currentFunction (Project { functions }) =
  G.toUnfoldable functions
    <#> \(Tuple name (Tuple function functionData)) ->
        let
          isCurrent = currentFunction == Just name

          isExternal = view _FunctionDataExternal functionData

          isEditable =
            not isCurrent
              && not isExternal
              && is _VisualFunction function

          isUsable = not isCurrent && isJust currentFunction
        in
          Tuple { name, function, functionData } { isUsable, isEditable }

onlyEditable :: Maybe FunctionName -> Project FunctionData NodeData -> Array (Tuple FunctionGraphNode NodeDescriptor)
onlyEditable c p = Array.filter (\(Tuple _ { isEditable }) -> isEditable) $ describe c p
