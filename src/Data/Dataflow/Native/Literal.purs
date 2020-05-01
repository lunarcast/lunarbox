module Lunarbox.Data.Dataflow.Native.Literal
  ( true'
  , false'
  , boolean
  , number
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Halogen.HTML as HH
import Halogen.HTML.Events (onValueInput)
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Switch (switch, switchHeight, switchWidth)
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), toBoolean, toNumber)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (typeBool, typeNumber)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.FunctionUi (FunctionUi)
import Svg.Attributes as SA
import Svg.Elements as SE

true' :: forall a s m. NativeConfig a s m
true' =
  NativeConfig
    { name: FunctionName "true"
    , expression: (NativeExpression (Forall [] typeBool) $ Bool true)
    , functionData: internal []
    , component: Nothing
    }

false' :: forall a s m. NativeConfig a s m
false' =
  NativeConfig
    { name: FunctionName "false"
    , expression: (NativeExpression (Forall [] typeBool) $ Bool false)
    , functionData: internal []
    , component: Nothing
    }

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
    , functionData: internal []
    , component: Just $ HH.lazy2 booleaUi
    }

numberUi :: forall a s m. FunctionUi a s m
numberUi { value } { setValue } =
  SE.foreignObject
    [ SA.height switchHeight
    , SA.width switchWidth
    , SA.x $ switchWidth / -2.0
    ]
    [ HH.input
        [ HP.value $ show $ toNumber value
        , HP.type_ HP.InputNumber
        , onValueInput $ setValue <<< Number <<< fromMaybe 0.0 <<< fromString
        ]
    ]

number :: forall a s m. NativeConfig a s m
number =
  NativeConfig
    { name: FunctionName "number"
    , expression: (NativeExpression (Forall [] typeNumber) $ Number 0.0)
    , functionData: internal []
    , component: Just $ HH.lazy2 numberUi
    }
