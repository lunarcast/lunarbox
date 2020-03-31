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

derive newtype instance showFunctionName :: Show FunctionName

instance expressibleNodeId :: Expressible FunctionName Unit where
  toExpression = toExpression <<< unwrap

_FunctionName :: Lens' FunctionName String
_FunctionName = newtypeIso
