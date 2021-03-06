module Lunarbox.Data.Editor.DataflowFunction
  ( DataflowFunction(..)
  , compileDataflowFunction
  , _VisualFunction
  , _NativeFunction
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Lunarbox.Data.Dataflow.Expression (Expression(..), NativeExpression)
import Lunarbox.Data.Editor.Class.Depends (class Depends, getDependencies)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Node.PinLocation (ScopedLocation(..))
import Lunarbox.Data.Editor.NodeGroup (NodeGroup, compileNodeGroup)

-- A dataflow function can either be:
-- 1) A native function
-- 2) A graph of nodes
data DataflowFunction
  = NativeFunction NativeExpression
  | VisualFunction NodeGroup

instance encodeJsonDataflowFunction :: EncodeJson DataflowFunction where
  encodeJson (VisualFunction nodeGroup) = "visual" := true ~> "function" := nodeGroup ~> jsonEmptyObject
  encodeJson (NativeFunction expression) = jsonEmptyObject

instance decodeJsonDataflowFunction :: DecodeJson DataflowFunction where
  decodeJson json = do
    obj <- decodeJson json
    nodeGroup <- obj .: "function"
    pure $ VisualFunction nodeGroup

instance dependencyDataflowFunction :: Depends DataflowFunction FunctionName where
  getDependencies (NativeFunction _) = mempty
  getDependencies (VisualFunction g) = getDependencies g

compileDataflowFunction :: DataflowFunction -> Expression ScopedLocation
compileDataflowFunction = case _ of
  NativeFunction f -> Native InsideNative f
  VisualFunction g -> compileNodeGroup g

-- Lenses
_VisualFunction :: Prism' DataflowFunction NodeGroup
_VisualFunction =
  prism' VisualFunction case _ of
    VisualFunction f -> Just f
    _ -> Nothing

_NativeFunction :: Prism' DataflowFunction NativeExpression
_NativeFunction =
  prism' NativeFunction case _ of
    NativeFunction f -> Just f
    _ -> Nothing
