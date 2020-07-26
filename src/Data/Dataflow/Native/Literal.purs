module Lunarbox.Data.Dataflow.Native.Literal
  ( literalNodes
  , boolean
  , number
  , string
  ) where

import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime.Class.Describable (toNativeExpression)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))

-- All the native literal nodes
literalNodes :: Array (NativeConfig)
literalNodes = [ boolean, number, string ]

boolean :: NativeConfig
boolean =
  NativeConfig
    { name: FunctionName "boolean"
    , expression: toNativeExpression false
    , functionData: internal [] { name: "Boolean", description: "A boolean which has the same value as the visual switch" }
    }

number :: NativeConfig
number =
  NativeConfig
    { name: FunctionName "number"
    , expression: toNativeExpression 0
    , functionData: internal [] { name: "Number", description: "A number which has the same value as the input box" }
    }

string :: NativeConfig
string =
  NativeConfig
    { name: FunctionName "string"
    , expression: toNativeExpression "lunarbox"
    , functionData: internal [] { name: "String", description: "A string which has the same value as the input textbox" }
    }
