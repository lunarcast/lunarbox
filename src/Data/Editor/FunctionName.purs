module Lunarbox.Data.Editor.FunctionName where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Default (class Default)
import Data.Lens (Lens')
import Data.Newtype (class Newtype, unwrap)
import Lunarbox.Data.Lens (newtypeIso)

newtype FunctionName
  = FunctionName String

derive instance eqFunctionName :: Eq FunctionName

derive instance ordFunctionName :: Ord FunctionName

derive instance newtypeFunctionName :: Newtype FunctionName _

derive newtype instance encodeJsonFunctionName :: EncodeJson FunctionName

derive newtype instance decodeJsonFunctionName :: DecodeJson FunctionName

instance defaultFunctionName :: Default FunctionName where
  def = FunctionName ""

instance showFunctionName :: Show FunctionName where
  show = unwrap

_FunctionName :: Lens' FunctionName String
_FunctionName = newtypeIso
