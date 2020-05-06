module Lunarbox.Data.Dataflow.Runtime
  ( RuntimeValue(..)
  , binaryFunction
  , toBoolean
  , toNumber
  , toString
  , _Number
  , _String
  , _Function
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (:=), (~>), (.:))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))
import Data.String (joinWith)

-- Representations of all possible runtime values
data RuntimeValue
  = Number Number
  | String String
  | Bool Boolean
  | NArray (Array RuntimeValue)
  | Null
  | Function (RuntimeValue -> RuntimeValue)

derive instance genericRuntimeValue :: Generic RuntimeValue _

instance encodeJsonRuntimeValue :: EncodeJson RuntimeValue where
  encodeJson (Number inner) = "type" := "number" ~> "value" := inner ~> jsonEmptyObject
  encodeJson (String inner) = "type" := "string" ~> "value" := inner ~> jsonEmptyObject
  encodeJson (Bool inner) = "type" := "boolean" ~> "value" := inner ~> jsonEmptyObject
  encodeJson (NArray inner) = "type" := "array" ~> "value" := inner ~> jsonEmptyObject
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
    Null -> "null"
    Bool value -> show value
    Number value -> show value
    String value -> show value
    NArray inner -> "[" <> joinWith ", " (show <$> inner) <> "]"
    Function value -> "Function"

instance eqRuntimeValue :: Eq RuntimeValue where
  eq (Number n) (Number n') = n == n'
  eq (String s) (String s') = s == s'
  eq (Bool v) (Bool v') = v == v'
  eq (NArray array) (NArray array') = array == array'
  eq Null Null = true
  eq _ _ = false

-- helper to ease the creation of binary functions
binaryFunction :: (RuntimeValue -> RuntimeValue -> RuntimeValue) -> RuntimeValue
binaryFunction f = Function $ Function <<< f

-- Turns any runtime value to a boolean
toBoolean :: RuntimeValue -> Boolean
toBoolean value
  | value == Bool true = true
  | otherwise = false

-- Extract a number from a runtime value, defaulting to 0
toNumber :: RuntimeValue -> Number
toNumber (Number inner) = inner

toNumber _ = 0.0

-- Similar to show except it doesn't put quotes around strings
toString :: RuntimeValue -> String
toString (String inner) = inner

toString other = show other

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
