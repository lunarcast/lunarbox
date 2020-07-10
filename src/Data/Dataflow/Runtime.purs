module Lunarbox.Data.Dataflow.Runtime
  ( RuntimeValue(..)
  , binaryFunction
  , ternaryFunction
  , toBoolean
  , toNumber
  , toString
  , toArray
  , strictEval
  , _Number
  , _String
  , _Function
  ) where

import Prelude
import Control.Lazy (class Lazy)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Debug.Trace (trace)

-- Representations of all possible runtime values
data RuntimeValue
  = Number Number
  | String String
  | Bool Boolean
  | NArray (Array RuntimeValue)
  | Null
  | Function (RuntimeValue -> RuntimeValue)
  | RLazy (Unit -> RuntimeValue)

derive instance genericRuntimeValue :: Generic RuntimeValue _

instance lazyRuntimeValue :: Lazy RuntimeValue where
  defer = RLazy

instance encodeJsonRuntimeValue :: EncodeJson RuntimeValue where
  encodeJson (Number inner) = "type" := "number" ~> "value" := inner ~> jsonEmptyObject
  encodeJson (String inner) = "type" := "string" ~> "value" := inner ~> jsonEmptyObject
  encodeJson (Bool inner) = "type" := "boolean" ~> "value" := inner ~> jsonEmptyObject
  encodeJson (NArray inner) = "type" := "array" ~> "value" := inner ~> jsonEmptyObject
  encodeJson (RLazy inner) = encodeJson $ inner unit
  encodeJson _ = "type" := "null" ~> jsonEmptyObject

instance decodeJsonRuntimeValue :: DecodeJson RuntimeValue where
  decodeJson json = do
    obj <- decodeJson json
    type' <- obj .: "type"
    case type' of
      "number" -> do
        value <- obj .: "value"
        pure $ Number value
      "string" -> do
        value <- obj .: "value"
        pure $ String value
      "boolean" -> do
        value <- obj .: "value"
        pure $ Bool value
      "array" -> do
        value <- obj .: "value"
        pure value
      "null" -> pure $ Null
      _ -> Left $ "Cannot parse runtime value of type " <> type'

instance showRuntimeValue :: Show RuntimeValue where
  show = case _ of
    Null -> ""
    Bool value -> if value then "True" else "False"
    Number value -> show value
    String value -> show value
    NArray inner -> "[" <> joinWith ", " (show <$> inner) <> "]"
    Function _ -> "Function"
    RLazy exec -> trace { exec } \_ -> "Lazy"

-- RLazy exec -> show $ exec unit
-- RLazy exec -> show $ exec unit
instance eqRuntimeValue :: Eq RuntimeValue where
  eq (Number n) (Number n') = n == n'
  eq (String s) (String s') = s == s'
  eq (Bool v) (Bool v') = v == v'
  eq (NArray array) (NArray array') = array == array'
  eq Null Null = true
  eq (RLazy a) (RLazy b) = a unit == b unit
  eq _ _ = false

instance ordRuntimeValue :: Ord RuntimeValue where
  compare (Number n) (Number n') = compare n n'
  compare (String s) (String s') = compare s s'
  compare (Bool v) (Bool v') = compare v v'
  compare (NArray array) (NArray array') = compare array array'
  compare (RLazy a) (RLazy b) = compare (a unit) (b unit)
  compare _ _ = EQ

-- helper to ease the creation of binary functions
binaryFunction :: (RuntimeValue -> RuntimeValue -> RuntimeValue) -> RuntimeValue
binaryFunction f = Function $ Function <<< f

-- Same as binaryFunction but with 3 arguments
ternaryFunction :: (RuntimeValue -> RuntimeValue -> RuntimeValue -> RuntimeValue) -> RuntimeValue
ternaryFunction f = Function $ binaryFunction <<< f

-- Turns any runtime value to a boolean
toBoolean :: RuntimeValue -> Boolean
toBoolean value
  | value == Bool true = true
  | otherwise = false

-- Extracts the array out of a runtime value. Returns [] if the vaule isn't an array
toArray :: RuntimeValue -> Array RuntimeValue
toArray (NArray inner) = inner

toArray _ = []

-- Extract a number from a runtime value, defaulting to 0
toNumber :: RuntimeValue -> Number
toNumber (Number inner) = inner

toNumber _ = 0.0

-- Similar to show except it doesn't put quotes around strings
toString :: RuntimeValue -> String
toString (String inner) = inner

toString other = show other

-- Unwrap all the lazies
strictEval :: RuntimeValue -> RuntimeValue
strictEval (RLazy exec) = strictEval $ exec unit

strictEval a = a

-- Lenses
_Number :: Prism' RuntimeValue Number
_Number =
  prism' Number case _ of
    Number c -> Just c
    _ -> Nothing

_String :: Prism' RuntimeValue String
_String =
  prism' String case _ of
    String c -> Just c
    _ -> Nothing

_Function :: Prism' RuntimeValue (RuntimeValue -> RuntimeValue)
_Function =
  prism' Function case _ of
    Function c -> Just c
    _ -> Nothing
