module Lunarbox.Data.Dataflow.Native.Literal (true', false', boolean) where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..))
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (typeBool)
import Lunarbox.Data.Editor.FunctionData (internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.FunctionUi (FunctionUi)

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
booleaUi { value } { setValue } = HH.text $ show value

boolean :: forall h a. NativeConfig h a
boolean =
  NativeConfig
    { name: FunctionName "boolean"
    , expression: (NativeExpression (Forall [] typeBool) $ Bool false)
    , functionData: internal []
    , component: Just booleaUi
    }
