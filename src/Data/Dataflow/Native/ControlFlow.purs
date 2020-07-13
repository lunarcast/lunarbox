module Lunarbox.Data.Dataflow.Native.ControlFlow
  ( controlFlowNodes
  ) where

import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), binaryFunction)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (TVarName(..), Type(..), typeBool, typeFunction)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Prelude (const, flip, ($))

-- All the native control flow nodes
controlFlowNodes :: Array (NativeConfig)
controlFlowNodes = [ if' ]

typeIf :: Scheme
typeIf = Forall [ return ] $ typeFunction typeBool $ typeFunction typeReturn $ typeFunction typeReturn typeReturn
  where
  return = TVarName "a"

  typeReturn = TVariable true return

evalIf :: RuntimeValue -> RuntimeValue
evalIf (Bool true) = binaryFunction const

evalIf (Bool false) = binaryFunction $ flip const

evalIf _ = Null

if' :: NativeConfig
if' =
  NativeConfig
    { name: FunctionName "if"
    , expression: (NativeExpression typeIf $ Function evalIf)
    , functionData:
      internal
        [ { name: "condition", description: "A boolean which decides what branch to evaluate to" }
        , { name: "then"
          , description: "A branch which will be chosem if the condition is true"
          }
        , { name: "else"
          , description: "A branch which will be chosen if the condition is false"
          }
        ]
        { name: "result"
        , description: "Evaluates to the 'then' argument if the condition is true, else this evaludates to the 'else' argument"
        }
    }
