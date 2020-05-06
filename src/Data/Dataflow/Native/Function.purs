module Lunarbox.Data.Dataflow.Native.Function (functionNodes) where

import Data.Maybe (Maybe(..))
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), binaryFunction)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (TVarName(..), Type(..), typeFunction)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Prelude (const, identity, ($), (>>>))

-- All the function related nodes from Prelude
functionNodes :: forall a s m. Array (NativeConfig a s m)
functionNodes = [ pipe, const', identity', compose, flip' ]

typePipe :: Scheme
typePipe =
  Forall [ input, output ] $ typeFunction (TVariable true input)
    $ typeFunction
        (typeFunction (TVariable true input) (TVariable true output))
        (TVariable true output)
  where
  input = TVarName "i"

  output = TVarName "o"

evalPipe :: RuntimeValue -> RuntimeValue -> RuntimeValue
evalPipe input (Function function) = function input

evalPipe _ _ = Null

pipe :: forall a s m. NativeConfig a s m
pipe =
  NativeConfig
    { name: FunctionName "pipe"
    , expression: (NativeExpression typePipe $ binaryFunction evalPipe)
    , functionData:
      internal
        [ { name: "input" }
        , { name: "function" }
        ]
        { name: "function(input)" }
    , component: Nothing
    }

typeIdentity :: Scheme
typeIdentity = Forall [ input ] $ typeFunction (TVariable true input) (TVariable true input)
  where
  input = TVarName "i"

identity' :: forall a s m. NativeConfig a s m
identity' =
  NativeConfig
    { name: FunctionName "identity"
    , expression: (NativeExpression typeIdentity $ Function identity)
    , functionData:
      internal
        [ { name: "x" }
        ]
        { name: "x" }
    , component: Nothing
    }

typeConst :: Scheme
typeConst = Forall [ input, ignore ] $ typeFunction (TVariable true input) $ typeFunction (TVariable true ignore) (TVariable true input)
  where
  input = TVarName "input"

  ignore = TVarName "ignore"

const' :: forall a s m. NativeConfig a s m
const' =
  NativeConfig
    { name: FunctionName "const"
    , expression: (NativeExpression typeConst $ binaryFunction const)
    , functionData:
      internal
        [ { name: "constant value" }
        , { name: "ignored value" }
        ]
        { name: "constant value" }
    , component: Nothing
    }

typeCompose :: Scheme
typeCompose = Forall [ a, b, c ] $ typeFunction (typeFunction typeA typeB) $ typeFunction (typeFunction typeB typeC) $ typeFunction typeA typeC
  where
  a = TVarName "t0"

  b = TVarName "t1"

  c = TVarName "t2"

  typeA = TVariable true a

  typeB = TVariable true b

  typeC = TVariable true c

evalCompose :: RuntimeValue -> RuntimeValue -> RuntimeValue
evalCompose (Function first) (Function second) = Function $ first >>> second

evalCompose _ _ = Null

compose :: forall a s m. NativeConfig a s m
compose =
  NativeConfig
    { name: FunctionName "compose"
    , expression: (NativeExpression typeCompose $ binaryFunction evalCompose)
    , functionData:
      internal
        [ { name: "first function" }
        , { name: "second function" }
        ]
        { name: "second . first" }
    , component: Nothing
    }

typeFlip :: Scheme
typeFlip = Forall [ a, b, c ] $ typeFunction (typeFunction typeA $ typeFunction typeB typeC) $ typeFunction typeB $ typeFunction typeA typeC
  where
  a = TVarName "t0"

  b = TVarName "t1"

  c = TVarName "t2"

  typeA = TVariable true a

  typeB = TVariable true b

  typeC = TVariable true c

evalFlip :: RuntimeValue -> RuntimeValue
evalFlip (Function function) =
  binaryFunction \first second -> case function second of
    Function inner -> inner first
    _ -> Null

evalFlip _ = Null

flip' :: forall a s m. NativeConfig a s m
flip' =
  NativeConfig
    { name: FunctionName "flip"
    , expression: (NativeExpression typeFlip $ Function evalFlip)
    , functionData:
      internal
        [ { name: "function" }
        ]
        { name: "flipped function" }
    , component: Nothing
    }
