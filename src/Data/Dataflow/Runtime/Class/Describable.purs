module Lunarbox.Data.Dataflow.Runtime.Class.Describable where

import Prelude
import Data.Array as Array
import Lunarbox.Data.Dataflow.Class.Substituable (ftv)
import Lunarbox.Data.Dataflow.Expression (NativeExpression(..))
import Lunarbox.Data.Dataflow.Runtime.Class.Runnable (class Corunnable, class Runnable, fromRuntime, toRuntime)
import Lunarbox.Data.Dataflow.Runtime.Class.Typeable (class Typeable, getType, typeof)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (Type)
import Type.Proxy (Proxy(..))

-- | Proxy for giving purs extra info about 
-- | how we want to transform someting into 
-- | lunarboxes type system.
-- | The first argument is a proof all ts are as
newtype DProxy t a
  = DProxy a

instance runnableDProxy :: Runnable a => Runnable (DProxy t a) where
  toRuntime (DProxy a) = toRuntime a

instance typeableDProxy :: Typeable t => Typeable (DProxy t a) where
  typeof _ = typeof (Proxy :: Proxy t)

instance corunnableDProxy :: Corunnable a => Corunnable (DProxy t a) where
  fromRuntime a = DProxy <$> fromRuntime a

-- | Typecalss for stuff which is both Typeable and Runnable
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
