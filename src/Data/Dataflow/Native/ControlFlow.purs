module Lunarbox.Data.Dataflow.Native.ControlFlow
  ( if'
  ) where

import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), binaryFunction)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (TVarName(..), Type(..), typeBool)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Prelude (const, flip, ($))

typeIf :: Scheme
typeIf = Forall [ return ] $ TArrow typeBool $ TArrow typeReturn $ TArrow typeReturn typeReturn
  where
  return = TVarName "a"

  typeReturn = TVarariable return

evalIf :: RuntimeValue -> RuntimeValue
evalIf (Bool true) = binaryFunction const

evalIf _ = binaryFunction $ flip const

if' :: NativeConfig
if' =
  NativeConfig
    { name: FunctionName "if"
    , expression: (NativeExpression typeIf $ Function evalIf)
    , functionData: internal [ { name: "condition" }, { name: "then" }, { name: "else" } ]
    }
