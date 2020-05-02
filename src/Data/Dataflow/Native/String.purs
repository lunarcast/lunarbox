module Lunarbox.Data.Dataflow.Native.String
  ( stringLength
  , concatStrings
  , reverseString
  , trimString
  ) where

import Prelude
import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String as String
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), binaryFunction)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (Type(..), typeNumber, typeString)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))

-- Helper for unary functions operating on strnigs
stringUnary :: (String -> String) -> RuntimeValue
stringUnary unary =
  Function case _ of
    String string -> String $ unary string
    _ -> Null

evalLength :: RuntimeValue -> RuntimeValue
evalLength (String string) = Number $ toNumber $ String.length string

evalLength _ = Null

stringLength :: forall a s m. NativeConfig a s m
stringLength =
  NativeConfig
    { name: FunctionName "length"
    , expression: NativeExpression (Forall [] $ TArrow typeString typeNumber) $ Function evalLength
    , functionData: internal [ { name: "string" } ]
    , component: Nothing
    }

evalConcat :: RuntimeValue -> RuntimeValue -> RuntimeValue
evalConcat (String first) (String second) = String $ first <> second

evalConcat _ _ = Null

concatStrings :: forall a s m. NativeConfig a s m
concatStrings =
  NativeConfig
    { name: FunctionName "concat"
    , expression: NativeExpression (Forall [] $ TArrow typeString $ TArrow typeString typeString) $ binaryFunction evalConcat
    , functionData: internal [ { name: "first string" }, { name: "second strings" } ]
    , component: Nothing
    }

evalReverse :: RuntimeValue
evalReverse = stringUnary $ String.fromCodePointArray <<< Array.reverse <<< String.toCodePointArray

reverseString :: forall a s m. NativeConfig a s m
reverseString =
  NativeConfig
    { name: FunctionName "reverse"
    , expression: NativeExpression (Forall [] $ TArrow typeString typeString) evalReverse
    , functionData: internal [ { name: "string" } ]
    , component: Nothing
    }

trimString :: forall a s m. NativeConfig a s m
trimString =
  NativeConfig
    { name: FunctionName "trim"
    , expression: NativeExpression (Forall [] $ TArrow typeString typeString) $ stringUnary String.trim
    , functionData: internal [ { name: "string" } ]
    , component: Nothing
    }
