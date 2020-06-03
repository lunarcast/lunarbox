module Lunarbox.Data.Editor.DataflowFunction
  ( DataflowFunction(..)
  , compileDataflowFunction
  , _VisualFunction
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Lunarbox.Data.Dataflow.Expression (Expression(..), NativeExpression(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..))
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (typeString)
import Lunarbox.Data.Editor.Class.Depends (class Depends, getDependencies)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..))
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Node.PinLocation (NodeOrPinLocation)
import Lunarbox.Data.Editor.NodeGroup (NodeGroup, compileNodeGroup)

-- A dataflow function can either be:
-- 1) A native function
-- 2) A graph of nodes
data DataflowFunction
  = NativeFunction NativeExpression
  | VisualFunction NodeGroup

instance encodeJsonDataflowFunction :: EncodeJson DataflowFunction where
  encodeJson (VisualFunction nodeGroup) = "visual" := true ~> "function" := nodeGroup ~> jsonEmptyObject
  encodeJson (NativeFunction expression) = "visual" := false ~> jsonEmptyObject

instance decodeJsonDataflowFunction :: DecodeJson DataflowFunction where
  decodeJson json = do
    obj <- decodeJson json
    visual <- obj .: "visual"
    if visual == true then do
      nodeGroup <- obj .: "function"
      pure $ VisualFunction nodeGroup
    else
      -- Create temporary funciton wchi should be replaced soon
      pure $ NativeFunction $ NativeExpression (Forall [] typeString) $ String "loading"

instance dependencyDataflowFunction :: Depends DataflowFunction FunctionName where
  getDependencies (NativeFunction _) = mempty
  getDependencies (VisualFunction g) = getDependencies g

compileDataflowFunction :: DataflowFunction -> Expression NodeOrPinLocation
compileDataflowFunction = case _ of
  NativeFunction f -> Native Nowhere f
  VisualFunction g -> compileNodeGroup g

_VisualFunction :: Prism' DataflowFunction NodeGroup
_VisualFunction =
  prism' VisualFunction case _ of
    VisualFunction f -> Just f
    _ -> Nothing
