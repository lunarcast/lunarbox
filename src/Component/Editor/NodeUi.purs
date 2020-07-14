module Lunarbox.Component.Editor.NodeUi
  ( runNodeUi
  , uiToRuntime
  , NodeUiInputs
  , NodeUi
  , hasUi
  ) where

import Prelude
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.Tuple (Tuple(..))
import Halogen (ComponentHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Switch (switch)
import Lunarbox.Component.Utils (className, maybeElement)
import Lunarbox.Data.Dataflow.Native.Literal (boolean, number, string)
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..))
import Lunarbox.Data.Dataflow.Runtime.Class.Runnable (fromRuntime)
import Lunarbox.Data.Editor.FunctionName (FunctionName)

type NodeUiInputs a
  = { setValue :: RuntimeValue -> Maybe a, value :: RuntimeValue }

type NodeUi a s m
  = { render :: NodeUiInputs a -> ComponentHTML a s m
    , finalize :: RuntimeValue -> Maybe RuntimeValue
    }

-- | Run an ui given its name and its inputs
runNodeUi :: forall a s m. NodeUiInputs a -> FunctionName -> ComponentHTML a s m
runNodeUi inputs name = maybeElement (Map.lookup name nodeUis) \{ render } -> render inputs

-- | Basically parse and save the value given by an ui
uiToRuntime :: FunctionName -> RuntimeValue -> Maybe RuntimeValue
uiToRuntime name value = Map.lookup name nodeUis >>= \{ finalize } -> finalize value

-- | Helper to not have to hardcode the names of nodes in 2 places
extendNativeConfig :: forall a s m. NativeConfig -> NodeUi a s m -> Tuple FunctionName (NodeUi a s m)
extendNativeConfig (NativeConfig { name }) = Tuple name

-- | List of all the node uis we have at the moment
nodeUis :: forall a s m. Map FunctionName (NodeUi a s m)
nodeUis = Map.fromFoldable [ stringNodeInput, numberNodeInput, booleanNodeInput ]

-- | Check if a function has some ui
hasUi :: FunctionName -> Boolean
hasUi = flip Map.member nodeUis

-- Those 3 are the actual uis we have atm
stringNodeInput :: forall a s m. Tuple FunctionName (NodeUi a s m)
stringNodeInput =
  extendNativeConfig string
    { render
    , finalize: Just
    }
  where
  render { value, setValue } =
    HH.input
      [ HP.value $ fromMaybe "" $ fromRuntime value
      , HP.type_ HP.InputText
      , className "node-input node-input--string"
      , HE.onValueInput $ setValue <<< String
      ]

numberNodeInput :: forall a s m. Tuple FunctionName (NodeUi a s m)
numberNodeInput =
  extendNativeConfig number
    { render
    , finalize: map Number <<< fromString <=< fromRuntime
    }
  where
  render { value, setValue } =
    HH.input
      [ HP.value case value of
          Number n -> show n
          other -> fromMaybe "" $ fromRuntime other
      , HP.type_ HP.InputNumber
      , className "node-input node-input--number"
      , HE.onValueInput $ setValue <<< String
      ]

booleanNodeInput :: forall a s m. Tuple FunctionName (NodeUi a s m)
booleanNodeInput =
  extendNativeConfig boolean
    { render
    , finalize: Just
    }
  where
  render { value, setValue } =
    switch
      { checked: fromMaybe false $ fromRuntime value
      , round: true
      }
      (setValue <<< Bool)
