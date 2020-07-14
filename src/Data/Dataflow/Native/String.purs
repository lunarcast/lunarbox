module Lunarbox.Data.Dataflow.Native.String
  ( stringNodes
  ) where

import Prelude
import Data.Array as Array
import Data.String as String
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime.Class.Describable (toNativeExpression)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))

-- List of all the string native nodes
stringNodes :: Array (NativeConfig)
stringNodes = [ stringLength, concatStrings, reverseString, trimString ]

stringLength :: NativeConfig
stringLength =
  NativeConfig
    { name: FunctionName "length"
    , expression: toNativeExpression String.length
    , functionData: internal [ { name: "string", description: "Any string" } ] { name: "length", description: "The number of characters in the given string" }
    }

concatStrings :: NativeConfig
concatStrings =
  NativeConfig
    { name: FunctionName "concat strings"
    , expression: toNativeExpression ((<>) :: String -> _)
    , functionData:
      internal
        [ { name: "first string", description: "Any string" }
        , { name: "second strings", description: "Any string" }
        ]
        { name: "a ++ b", description: "The result of 'glueing' the strings together" }
    }

reverseString :: NativeConfig
reverseString =
  NativeConfig
    { name: FunctionName "reverse"
    , expression: toNativeExpression $ String.fromCodePointArray <<< Array.reverse <<< String.toCodePointArray
    , functionData: internal [ { name: "string", description: "Any string" } ] { name: "reversed string", description: "The given string in reverse" }
    }

trimString :: NativeConfig
trimString =
  NativeConfig
    { name: FunctionName "trim"
    , expression: toNativeExpression String.trim
    , functionData:
      internal [ { name: "string", description: "Any string" } ]
        { name: "trimmed string", description: "The given string but without spaces at the end and at the start"
        }
    }
