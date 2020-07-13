module Lunarbox.Data.Dataflow.Native.String
  ( stringNodes
  ) where

import Prelude
import Data.Array as Array
import Data.Int (toNumber)
import Data.String as String
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), binaryFunction)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (typeFunction, typeNumber, typeString)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))

-- List of all the string native nodes
stringNodes :: Array (NativeConfig)
stringNodes = [ stringLength, concatStrings, reverseString, trimString ]

-- Helper for unary functions operating on strnigs
stringUnary :: (String -> String) -> RuntimeValue
stringUnary unary =
  Function case _ of
    String string -> String $ unary string
    _ -> Null

evalLength :: RuntimeValue -> RuntimeValue
evalLength (String string) = Number $ toNumber $ String.length string

evalLength _ = Null

stringLength :: NativeConfig
stringLength =
  NativeConfig
    { name: FunctionName "length"
    , expression: NativeExpression (Forall [] $ typeFunction typeString typeNumber) $ Function evalLength
    , functionData: internal [ { name: "string", description: "Any string" } ] { name: "length", description: "The number of characters in the given string" }
    }

evalConcat :: RuntimeValue -> RuntimeValue -> RuntimeValue
evalConcat (String first) (String second) = String $ first <> second

evalConcat _ _ = Null

concatStrings :: NativeConfig
concatStrings =
  NativeConfig
    { name: FunctionName "concat strings"
    , expression: NativeExpression (Forall [] $ typeFunction typeString $ typeFunction typeString typeString) $ binaryFunction evalConcat
    , functionData:
      internal
        [ { name: "first string", description: "Any string" }
        , { name: "second strings", description: "Any string" }
        ]
        { name: "a ++ b", description: "The result of 'glueing' the strings together" }
    }

evalReverse :: RuntimeValue
evalReverse = stringUnary $ String.fromCodePointArray <<< Array.reverse <<< String.toCodePointArray

reverseString :: NativeConfig
reverseString =
  NativeConfig
    { name: FunctionName "reverse"
    , expression: NativeExpression (Forall [] $ typeFunction typeString typeString) evalReverse
    , functionData: internal [ { name: "string", description: "Any string" } ] { name: "reversed string", description: "The given string in reverse" }
    }

trimString :: NativeConfig
trimString =
  NativeConfig
    { name: FunctionName "trim"
    , expression: NativeExpression (Forall [] $ typeFunction typeString typeString) $ stringUnary String.trim
    , functionData:
      internal [ { name: "string", description: "Any string" } ]
        { name: "trimmed string", description: "The given string but without spaces at the end and at the start"
        }
    }
