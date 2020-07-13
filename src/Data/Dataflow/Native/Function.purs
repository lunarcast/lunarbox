module Lunarbox.Data.Dataflow.Native.Function (functionNodes) where

import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), binaryFunction)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (TVarName(..), Type(..), typeFunction)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Prelude (const, identity, ($), (>>>))

-- All the function related nodes from Prelude
functionNodes :: Array (NativeConfig)
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

pipe :: NativeConfig
pipe =
  NativeConfig
    { name: FunctionName "pipe"
    , expression: (NativeExpression typePipe $ binaryFunction evalPipe)
    , functionData:
      internal
        [ { name: "input"
          , description: "The argument to pass to the given function"
          }
        , { name: "function"
          , description: "A function to call with the given argument"
          }
        ]
        { name: "output", description: "Calls the given function with the given argument" }
    }

typeIdentity :: Scheme
typeIdentity = Forall [ input ] $ typeFunction (TVariable true input) (TVariable true input)
  where
  input = TVarName "i"

identity' :: NativeConfig
identity' =
  NativeConfig
    { name: FunctionName "identity"
    , expression: (NativeExpression typeIdentity $ Function identity)
    , functionData:
      internal
        [ { name: "x"
          , description: "Any value"
          }
        ]
        { name: "x", description: "The given value" }
    }

typeConst :: Scheme
typeConst = Forall [ input, ignore ] $ typeFunction (TVariable true input) $ typeFunction (TVariable true ignore) (TVariable true input)
  where
  input = TVarName "input"

  ignore = TVarName "ignore"

const' :: NativeConfig
const' =
  NativeConfig
    { name: FunctionName "const"
    , expression: (NativeExpression typeConst $ binaryFunction const)
    , functionData:
      internal
        [ { name: "constant value"
          , description: "A value to always return"
          }
        , { name: "ignored value"
          , description: "Any value"
          }
        ]
        { name: "constant value", description: "The constant value passed to this function" }
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

compose :: NativeConfig
compose =
  NativeConfig
    { name: FunctionName "compose"
    , expression: (NativeExpression typeCompose $ binaryFunction evalCompose)
    , functionData:
      internal
        [ { name: "first function"
          , description: "A function from type A to type B"
          }
        , { name: "second function", description: "A function from type B and type C" }
        ]
        { name: "second . first", description: "A function which first applies the A -> B transformation and then passes the return of that to the B -> C transformation returning a value of type C" }
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

flip' :: NativeConfig
flip' =
  NativeConfig
    { name: FunctionName "flip"
    , expression: (NativeExpression typeFlip $ Function evalFlip)
    , functionData:
      internal
        [ { name: "function"
          , description: "A function which takes 2 arguments"
          }
        ]
        { name: "flipped function"
        , description: "A function which does the same things as the input function but with the arguments flipped around"
        }
    }
