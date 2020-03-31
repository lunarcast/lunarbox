module Lunarbox.Data.Editor.DataflowFunction
  ( DataflowFunction(..)
  , _VisualFunction
  ) where

import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Lunarbox.Data.Dataflow.Expression (NativeExpression)
import Lunarbox.Data.Editor.NodeGroup (NodeGroup)

-- A dataflow function can either be:
-- 1) A native function
-- 2) A graph of nodes
data DataflowFunction a
  = NativeFunction NativeExpression
  | VisualFunction (NodeGroup a)

_VisualFunction :: forall a. Prism' (DataflowFunction a) (NodeGroup a)
_VisualFunction =
  prism' VisualFunction case _ of
    VisualFunction f -> Just f
    _ -> Nothing
