module Lunarbox.Data.Dataflow.Native.Prelude
  ( configs
  , loadPrelude
  ) where

import Lunarbox.Data.Dataflow.Native.ControlFlow (if')
import Lunarbox.Data.Dataflow.Native.Function (const', identity', pipe)
import Lunarbox.Data.Dataflow.Native.Literal (boolean, false', number, string, true')
import Lunarbox.Data.Dataflow.Native.Logic (and, not', or, xor)
import Lunarbox.Data.Dataflow.Native.Math (add)
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig, loadNativeConfigs)
import Lunarbox.Data.Dataflow.Native.String (concatStrings, reverseString, stringLength)
import Lunarbox.Data.Editor.State (State)

-- Array wita s mll the built in nodes
configs :: forall a s m. Array (NativeConfig a s m)
configs =
  [ add
  , if'
  , pipe
  , identity'
  , const'
  , true'
  , false'
  , boolean
  , number
  , string
  , not'
  , and
  , or
  , xor
  -- String helpers 
  , reverseString
  , concatStrings
  , stringLength
  ]

-- Load all the built in nodes
loadPrelude :: forall a s m. State a s m -> State a s m
loadPrelude = loadNativeConfigs configs
