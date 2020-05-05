module Lunarbox.Data.Dataflow.Native.ControlFlow
  ( controlFlowNodes
  ) where

import Data.Maybe (Maybe(..))
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), binaryFunction)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (TVarName(..), Type(..), typeBool)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Prelude (const, flip, ($))

-- All the native control flow nodes
controlFlowNodes :: forall a s m. Array (NativeConfig a s m)
controlFlowNodes = [ if' ]

typeIf :: Scheme
typeIf = Forall [ return ] $ TArrow typeBool $ TArrow typeReturn $ TArrow typeReturn typeReturn
  where
  return = TVarName "a"

  typeReturn = TVariable true return

evalIf :: RuntimeValue -> RuntimeValue
evalIf (Bool true) = binaryFunction const

evalIf (Bool false) = binaryFunction $ flip const

evalIf _ = Null

if' :: forall a s m. NativeConfig a s m
if' =
  NativeConfig
    { name: FunctionName "if"
    , expression: (NativeExpression typeIf $ Function evalIf)
    , functionData: internal [ { name: "condition" }, { name: "then" }, { name: "else" } ] { name: "result" }
    , component: Nothing
    }
