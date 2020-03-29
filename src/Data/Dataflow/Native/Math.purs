module Lunarbox.Data.Dataflow.Native.Math where

import Prelude (($), (+))
import Lunarbox.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Dataflow.Type (Type(..), typeNumber)

addT :: Type
addT = TArrow typeNumber $ TArrow typeNumber typeNumber
 -- add :: NativeExpression Number -- add = NativeExpression addT (\a -> \b -> a + b)