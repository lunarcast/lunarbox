module Lunarbox.Data.Editor.FunctionName where

import Prelude
import Data.Default (class Default)
import Data.Lens (Lens')
import Data.Newtype (class Newtype, unwrap)
import Lunarbox.Data.Dataflow.Class.Expressible (class Expressible, toExpression)
import Lunarbox.Data.Lens (newtypeIso)

newtype FunctionName
  = FunctionName String

derive instance eqFunctionName :: Eq FunctionName

derive instance ordFunctionName :: Ord FunctionName

derive instance newtypeFunctionName :: Newtype FunctionName _

instance defaultFunctionName :: Default FunctionName where
  def = FunctionName ""

instance showFunctionName :: Show FunctionName where
  show = unwrap

instance expressibleNodeId :: Expressible FunctionName Unit where
  toExpression = toExpression <<< unwrap

_FunctionName :: Lens' FunctionName String
_FunctionName = newtypeIso
