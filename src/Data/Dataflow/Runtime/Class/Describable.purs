module Lunarbox.Data.Dataflow.Runtime.Class.Describable where

import Prelude
import Data.Array as Array
import Lunarbox.Data.Dataflow.Class.Substituable (ftv)
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Runtime.Class.Runnable (class Runnable, toRuntime)
import Lunarbox.Data.Dataflow.Runtime.Class.Typeable (class Typeable, getType)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (Type)

class (Runnable a, Typeable a) <= Describable a

instance describableA :: (Runnable a, Typeable a) => Describable a

-- | Quantify over all free variables in a type
generalizeType :: Type -> Scheme
generalizeType ty = Forall (Array.fromFoldable $ ftv ty) ty

-- | generate a native expression from a purescript value
toNativeExpression :: forall a. Describable a => a -> NativeExpression
toNativeExpression value = NativeExpression ty runtimeValue
  where
  ty = generalizeType $ getType value

  runtimeValue = toRuntime value
