module Lunarbox.Data.Dataflow.FunctionName where

import Prelude
import Data.Lens (Lens')
import Data.Newtype (class Newtype, unwrap)
import Lunarbox.Data.Lens (newtypeIso)
import Lunarbox.Dataflow.Expressible (class Expressible)

newtype FunctionName
  = FunctionName String

derive instance eqFunctionName :: Eq FunctionName

derive instance ordFunctionName :: Ord FunctionName

derive instance newtypeFunctionName :: Newtype FunctionName _

derive newtype instance expressibleFunctionName :: Expressible FunctionName

instance showFunctionName :: Show FunctionName where
  show = unwrap

_FunctionName :: Lens' FunctionName String
_FunctionName = newtypeIso
