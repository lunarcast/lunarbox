module Lunarbox.Data.Dataflow.FunctionName where

import Prelude
import Data.Lens (Lens')
import Data.Newtype (class Newtype, unwrap)
import Lunarbox.Data.Dataflow.Class.Expressible (class Expressible, toExpression)
import Lunarbox.Data.Lens (newtypeIso)

newtype FunctionName
  = FunctionName String

derive instance eqFunctionName :: Eq FunctionName

derive instance ordFunctionName :: Ord FunctionName

derive instance newtypeFunctionName :: Newtype FunctionName _

instance expressibleFunctionName :: Expressible FunctionName Unit where
  toExpression = toExpression <<< unwrap

instance showFunctionName :: Show FunctionName where
  show = unwrap

_FunctionName :: Lens' FunctionName String
_FunctionName = newtypeIso
