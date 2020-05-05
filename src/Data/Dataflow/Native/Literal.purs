module Lunarbox.Data.Dataflow.Native.Literal
  ( literalNodes
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Halogen.HTML as HH
import Halogen.HTML.Events (onValueInput)
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Switch (switch, switchHeight, switchWidth)
import Lunarbox.Component.Utils (className)
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), toBoolean, toNumber, toString)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (typeBool, typeNumber, typeString)
import Lunarbox.Data.Editor.Constants (inputWIdth)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.FunctionUi (FunctionUi)
import Svg.Attributes as SA
import Svg.Elements as SE

-- All the native literal nodes
literalNodes :: forall a s m. Array (NativeConfig a s m)
literalNodes = [ boolean, number, string ]

booleaUi ::
  forall a s m. FunctionUi a s m
booleaUi { value } { setValue } =
  SE.foreignObject
    [ SA.height switchHeight
    , SA.width switchWidth
    , SA.x $ switchWidth / -2.0
    ]
    [ switch { checked: toBoolean value, round: true } (setValue <<< Bool)
    ]

boolean :: forall a s m. NativeConfig a s m
boolean =
  NativeConfig
    { name: FunctionName "boolean"
    , expression: (NativeExpression (Forall [] typeBool) $ Bool false)
    , functionData: internal [] { name: "Boolean" }
    , component: Just $ HH.lazy2 booleaUi
    }

numberUi :: forall a s m. FunctionUi a s m
numberUi { value } { setValue } =
  SE.foreignObject
    [ SA.height switchHeight
    , SA.width inputWIdth
    , SA.x $ inputWIdth / -2.0
    ]
    [ HH.input
        [ HP.value $ show $ toNumber value
        , HP.type_ HP.InputNumber
        , className "number node-input"
        , onValueInput $ setValue <=< map Number <<< fromString
        ]
    ]

number :: forall a s m. NativeConfig a s m
number =
  NativeConfig
    { name: FunctionName "number"
    , expression: (NativeExpression (Forall [] typeNumber) $ Number 0.0)
    , functionData: internal [] { name: "Number" }
    , component: Just $ HH.lazy2 numberUi
    }

stringUI :: forall a s m. FunctionUi a s m
stringUI { value } { setValue } =
  SE.foreignObject
    [ SA.height switchHeight
    , SA.width inputWIdth
    , SA.x $ inputWIdth / -2.0
    ]
    [ HH.input
        [ HP.value $ toString value
        , HP.type_ HP.InputText
        , className "string node-input"
        , onValueInput $ setValue <<< String
        ]
    ]

string :: forall a s m. NativeConfig a s m
string =
  NativeConfig
    { name: FunctionName "string"
    , expression: (NativeExpression (Forall [] typeString) $ String "lunarbox")
    , functionData: internal [] { name: "String" }
    , component: Just $ HH.lazy2 stringUI
    }
