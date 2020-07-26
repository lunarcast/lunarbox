module Lunarbox.Data.Dataflow.Runtime.Class.Typeable where

import Prelude
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Lunarbox.Data.Dataflow.Type (TVarName(..), Type(..), typeArray, typeBool, typeFunction, typeNumber, typeString)
import Type.Proxy (Proxy(..))

class Typeable (a :: Type) where
  typeof :: Proxy a -> Type

instance typeableNumber :: Typeable Number where
  typeof _ = typeNumber

instance typeableInt :: Typeable Int where
  typeof _ = typeNumber

instance typeableString :: Typeable String where
  typeof _ = typeString

instance typeableBool :: Typeable Boolean where
  typeof _ = typeBool

instance typeableArray :: Typeable a => Typeable (Array a) where
  typeof _ = typeArray (typeof (Proxy :: Proxy a))

instance typeableSymbol :: IsSymbol sym => Typeable (SProxy sym) where
  typeof _ = TVariable true $ TVarName $ reflectSymbol (SProxy :: SProxy sym)

instance typeableArrow :: (Typeable a, Typeable b) => Typeable (a -> b) where
  typeof _ = typeFunction (typeof (Proxy :: Proxy a)) (typeof (Proxy :: Proxy b))

-- | Get the type of a purescript value
getType :: forall a. Typeable a => a -> Type
getType _ = typeof (Proxy :: Proxy a)
