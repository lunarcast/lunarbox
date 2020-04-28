module Lunarbox.Data.Dataflow.Native.Literal (true', false', boolean) where

import Prelude
import Data.Maybe (Maybe(..))
import Lunarbox.Component.Switch (switch, switchHeight, switchWidth)
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), toBoolean)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (typeBool)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.FunctionUi (FunctionUi)
import Svg.Attributes as SA
import Svg.Elements as SE

true' :: forall h a. NativeConfig h a
true' =
  NativeConfig
    { name: FunctionName "true"
    , expression: (NativeExpression (Forall [] typeBool) $ Bool true)
    , functionData: internal []
    , component: Nothing
    }

false' :: forall h a. NativeConfig h a
false' =
  NativeConfig
    { name: FunctionName "false"
    , expression: (NativeExpression (Forall [] typeBool) $ Bool false)
    , functionData: internal []
    , component: Nothing
    }

booleaUi ::
  forall h a. FunctionUi h a
booleaUi { value } { setValue } =
  SE.foreignObject
    [ SA.height switchHeight
    , SA.width switchWidth
    , SA.x $ switchWidth / -2.0
    ]
    [ switch { checked: toBoolean value, round: true } (setValue <<< Bool)
    ]

boolean :: forall h a. NativeConfig h a
boolean =
  NativeConfig
    { name: FunctionName "boolean"
    , expression: (NativeExpression (Forall [] typeBool) $ Bool false)
    , functionData: internal []
    , component: Just booleaUi
    }
