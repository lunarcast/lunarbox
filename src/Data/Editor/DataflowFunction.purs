module Lunarbox.Data.Editor.DataflowFunction
  ( DataflowFunction(..)
  , compileDataflowFunction
  , _VisualFunction
  ) where

import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Lunarbox.Data.Dataflow.Expression (Expression(..), NativeExpression)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..))
import Lunarbox.Data.Editor.Node.PinLocation (NodeOrPinLocation)
import Lunarbox.Data.Editor.NodeGroup (NodeGroup, compileNodeGroup)

-- A dataflow function can either be:
-- 1) A native function
-- 2) A graph of nodes
data DataflowFunction
  = NativeFunction NativeExpression
  | VisualFunction NodeGroup

compileDataflowFunction :: DataflowFunction -> Expression NodeOrPinLocation
compileDataflowFunction = case _ of
  NativeFunction f -> Native Nowhere f
  VisualFunction g -> compileNodeGroup g

_VisualFunction :: Prism' DataflowFunction NodeGroup
_VisualFunction =
  prism' VisualFunction case _ of
    VisualFunction f -> Just f
    _ -> Nothing
